#if defined(_MSC_VER)
//  Microsoft VC++
#define EXPORT __declspec(dllexport)
#else
//  GCC
#define EXPORT __attribute__((visibility("default")))
#endif

EXPORT void initializeOcaml(void);
