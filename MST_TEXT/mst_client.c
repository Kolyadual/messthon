#include <tox/tox.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

#define PROFILE_FILE "messthon.tox"
#define MAX_INPUT 2048

static Tox *tox;
static int running = 1;

// Публичные bootstrap-узлы (для подключения к глобальной сети)
static const char *nodes[][3] = {
    {"192.254.75.98", "33445", "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074FA6C0C"},
    {"144.76.60.215", "33445", "04119E835DF3E78BACF0F84235B300546AF8B936F035185E2A8E9E0A67C8924F"},
    {"5.189.176.217", "33445", "F5A1A38EFB6BD3C2C8F8B10D8406B524971C779DFB8B96368A7D8B5D2A4A5D3C"},
    {NULL, NULL, NULL}
};

// ------------------------------------------------------------
// Обработчики событий Tox
// ------------------------------------------------------------
void handle_friend_message(Tox *tox, uint32_t friend_number, TOX_MESSAGE_TYPE type,
                           const uint8_t *message, size_t length, void *user_data) {
    printf("\n[Друг %u] %.*s\n> ", friend_number, (int)length, message);
    fflush(stdout);
                           }

                           void handle_friend_request(Tox *tox, const uint8_t *public_key, const uint8_t *message,
                                                      size_t length, void *user_data) {
                               printf("\nЗапрос в друзья: %.*s\nПринять? (y/n): ", (int)length, message);
                               fflush(stdout);

                               char answer = getchar();
                               while (getchar() != '\n'); // очистка буфера

                               if (answer == 'y' || answer == 'Y') {
                                   Tox_Err_Friend_Add err_add;
                                   tox_friend_add_norequest(tox, public_key, &err_add);
                                   if (err_add == TOX_ERR_FRIEND_ADD_OK) {
                                       printf("Друг добавлен!\n");
                                   } else {
                                       printf("Ошибка добавления: %d\n", err_add);
                                   }
                               }
                                                      }

                                                      // ------------------------------------------------------------
                                                      // Загрузка/сохранение профиля
                                                      // ------------------------------------------------------------
                                                      void load_profile() {
                                                          FILE *f = fopen(PROFILE_FILE, "rb");
                                                          if (!f) {
                                                              tox = tox_new(NULL, NULL);
                                                              return;
                                                          }

                                                          fseek(f, 0, SEEK_END);
                                                          size_t size = ftell(f);
                                                          rewind(f);

                                                          uint8_t *data = malloc(size);
                                                          fread(data, 1, size, f);
                                                          fclose(f);

                                                          struct Tox_Options opts;
                                                          tox_options_default(&opts);
                                                          opts.savedata_type = TOX_SAVEDATA_TYPE_TOX_SAVE;
                                                          opts.savedata_data = data;
                                                          opts.savedata_length = size;

                                                          Tox_Err_New err_new;
                                                          tox = tox_new(&opts, &err_new);
                                                          free(data);

                                                          if (err_new != TOX_ERR_NEW_OK || !tox) {
                                                              fprintf(stderr, "Ошибка загрузки профиля, создаём новый.\n");
                                                              tox = tox_new(NULL, &err_new);
                                                          }
                                                      }

                                                      void save_profile() {
                                                          size_t size = tox_get_savedata_size(tox);
                                                          uint8_t *data = malloc(size);
                                                          tox_get_savedata(tox, data);

                                                          FILE *f = fopen(PROFILE_FILE, "wb");
                                                          if (f) {
                                                              fwrite(data, 1, size, f);
                                                              fclose(f);
                                                          } else {
                                                              fprintf(stderr, "Не удалось сохранить профиль!\n");
                                                          }
                                                          free(data);
                                                      }

                                                      // ------------------------------------------------------------
                                                      // Подключение к bootstrap-узлам
                                                      // ------------------------------------------------------------
                                                      void bootstrap() {
                                                          for (int i = 0; nodes[i][0] != NULL; i++) {
                                                              uint8_t key_bin[32];
                                                              for (int j = 0; j < 32; j++) {
                                                                  sscanf(nodes[i][2] + 2*j, "%2hhx", &key_bin[j]);
                                                              }
                                                              Tox_Err_Bootstrap err;
                                                              tox_bootstrap(tox, nodes[i][0], atoi(nodes[i][1]), key_bin, &err);
                                                              if (err != TOX_ERR_BOOTSTRAP_OK) {
                                                                  fprintf(stderr, "Ошибка bootstrap %s:%d - %d\n", nodes[i][0], atoi(nodes[i][1]), err);
                                                              } else {
                                                                  printf("Bootstrap %s:%d OK\n", nodes[i][0], atoi(nodes[i][1]));
                                                              }
                                                          }
                                                      }

                                                      // ------------------------------------------------------------
                                                      // Поток для чтения ввода с клавиатуры
                                                      // ------------------------------------------------------------
                                                      void *input_thread(void *arg) {
                                                          char line[MAX_INPUT];
                                                          while (running) {
                                                              printf("> ");
                                                              fflush(stdout);
                                                              if (!fgets(line, sizeof(line), stdin)) break;
                                                              line[strcspn(line, "\n")] = 0;

                                                              if (strncmp(line, "/add ", 5) == 0) {
                                                                  const char *id_hex = line + 5;
                                                                  if (strlen(id_hex) != 76) {
                                                                      printf("ID должен быть 76 символов!\n");
                                                                      continue;
                                                                  }
                                                                  uint8_t id_bin[TOX_ADDRESS_SIZE];
                                                                  for (int i = 0; i < TOX_ADDRESS_SIZE; i++) {
                                                                      sscanf(id_hex + 2*i, "%2hhx", &id_bin[i]);
                                                                  }
                                                                  Tox_Err_Friend_Add err_add;
                                                                  uint32_t friend_num = tox_friend_add(tox, id_bin, (uint8_t*)"Привет! Давай дружить", 22, &err_add);
                                                                  if (err_add == TOX_ERR_FRIEND_ADD_OK) {
                                                                      printf("Друг добавлен, номер %u\n", friend_num);
                                                                      save_profile();
                                                                  } else {
                                                                      printf("Ошибка добавления: %d\n", err_add);
                                                                  }
                                                              }
                                                              else if (strncmp(line, "/msg ", 5) == 0) {
                                                                  int friend_num;
                                                                  char msg[2000];
                                                                  if (sscanf(line, "/msg %d %[^\n]", &friend_num, msg) == 2) {
                                                                      Tox_Err_Friend_Send_Message err_send;
                                                                      tox_friend_send_message(tox, friend_num, TOX_MESSAGE_TYPE_NORMAL,
                                                                                              (uint8_t*)msg, strlen(msg), &err_send);
                                                                      if (err_send != TOX_ERR_FRIEND_SEND_MESSAGE_OK) {
                                                                          printf("Ошибка отправки: %d\n", err_send);
                                                                      } else {
                                                                          printf("(отправлено)\n");
                                                                      }
                                                                  } else {
                                                                      printf("Формат: /msg <номер> <текст>\n");
                                                                  }
                                                              }
                                                              else if (strcmp(line, "/list") == 0) {
                                                                  size_t count = tox_self_get_friend_list_size(tox);
                                                                  uint32_t *list = malloc(count * sizeof(uint32_t));
                                                                  tox_self_get_friend_list(tox, list);
                                                                  printf("Друзья (%zu):\n", count);
                                                                  for (size_t i = 0; i < count; i++) {
                                                                      uint32_t fnum = list[i];
                                                                      uint8_t name[TOX_MAX_NAME_LENGTH];
                                                                      Tox_Err_Friend_Query err_name;
                                                                      size_t name_len = tox_friend_get_name_size(tox, fnum, &err_name);
                                                                      if (err_name == TOX_ERR_FRIEND_QUERY_OK) {
                                                                          tox_friend_get_name(tox, fnum, name, &err_name);
                                                                          printf("  %u: %.*s\n", fnum, (int)name_len, name);
                                                                      } else {
                                                                          printf("  %u: <unknown>\n", fnum);
                                                                      }
                                                                  }
                                                                  free(list);
                                                              }
                                                              else if (strcmp(line, "/exit") == 0) {
                                                                  running = 0;
                                                                  break;
                                                              }
                                                              else if (strlen(line) > 0) {
                                                                  printf("Неизвестная команда. Доступно: /add <id>, /msg <номер> <текст>, /list, /exit\n");
                                                              }
                                                          }
                                                          return NULL;
                                                      }

                                                      // ------------------------------------------------------------
                                                      // MAIN
                                                      // ------------------------------------------------------------
                                                      int main() {
                                                          load_profile();
                                                          if (!tox) {
                                                              fprintf(stderr, "Не удалось создать Tox\n");
                                                              return 1;
                                                          }

                                                          uint8_t address[TOX_ADDRESS_SIZE];
                                                          tox_self_get_address(tox, address);
                                                          printf("Мой Tox ID: ");
                                                          for (size_t i = 0; i < TOX_ADDRESS_SIZE; i++) {
                                                              printf("%02X", address[i]);
                                                          }
                                                          printf("\n");

                                                          tox_callback_friend_message(tox, handle_friend_message);
                                                          tox_callback_friend_request(tox, handle_friend_request);

                                                          bootstrap();

                                                          pthread_t thread;
                                                          pthread_create(&thread, NULL, input_thread, NULL);

                                                          while (running) {
                                                              tox_iterate(tox, NULL);
                                                              usleep(tox_iteration_interval(tox) * 1000);
                                                          }

                                                          pthread_join(thread, NULL);
                                                          save_profile();
                                                          tox_kill(tox);
                                                          printf("Всего доброго!\n");
                                                          return 0;
                                                      }
