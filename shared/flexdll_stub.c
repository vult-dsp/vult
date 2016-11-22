
void flexdll_dump_exports(void* u){(void)u;}
void *flexdll_dlopen(char const* file, int mode){(void)file; (void)mode; return (void*)0;}
void flexdll_dlclose(void* u){(void)u;}
void* flexdll_dlsym(void* u, const char * n) {(void)u; (void)n; return (void*)0;}
char* flexdll_dlerror(){static char* flexdll_error_buffer = "flexdll is not availble"; return flexdll_error_buffer;}