# Benchmark fixture tables for pi-coding-agent

Tables below are extracted by the bench harness.  Each table is
separated by at least one blank line of non-table text.  The fixture
contains three sets of six table shapes (18 tables total) to push
timings well above the CI noise floor.

## Set 1: Simple wide table

A basic table with plain text cells that are wide enough to trigger
column wrapping at typical terminal widths.

| Name | Age | City | Occupation | Notes |
| :-- | --: | :--: | :-- | :-- |
| Alice Johnson | 34 | San Francisco, California | Senior Software Engineer at Cloudflare | Has been writing Rust since 2019 |
| Bob Williams | 28 | New York City, New York | Data Scientist and ML Researcher | Published three papers on transformers |
| Charlie Brown | 45 | London, United Kingdom | Chief Technology Officer at Fintech | Previously at Google and Amazon |
| Diana Prince | 31 | Tokyo, Japan | Product Manager at AI Startup | Fluent in English and Japanese |
| Eve Martinez | 52 | Berlin, Germany | University Professor of Computer Science | 24 years teaching experience |

## Set 1: Inline markdown table

Rich inline formatting: bold, italic, code, links, strikethrough,
and bold-italic combinations.

| Title | Year | Publisher | Primary Thesis | Recognition |
| :-- | --: | :--: | :-- | :-- |
| ***Democracy for the Few*** | 1974 | *St. Martin's Press* (9 editions through 2011) | American government systematically serves **corporate and wealthy interests** while offering only *symbolic concessions* to the working majority | Adopted as a textbook at over **300 universities**; [reviewed in *APSR*](https://example.com/review) |
| **Inventing Reality** | 1986 | *St. Martin's Press* | Mainstream media operate not through overt **censorship** but via structural filters — **ownership concentration**, advertising dependency, and reliance on `official sources` | Preceded Chomsky & Herman's *Manufacturing Consent* by two years; cited in over `1,200` scholarly works |
| **Against Empire** | 1995 | *City Lights Books* | US foreign policy constitutes a coherent system of **imperial extraction** — not a series of *mistakes* — designed to maintain **access to cheap labor** and raw materials | Part of the [City Lights Open Media Series](https://citylights.com); **ISBN** `978-0-87286-298-2`; translated into 8 languages |
| ***The Face of Imperialism*** | 2011 | *Paradigm Publishers* | Imperialism operates through a **triad of control** — military force, international financial institutions (`IMF`, `World Bank`, `WTO`), and *ideological hegemony* | Final major theoretical work; [reviewed in *Monthly Review*](https://monthlyreview.org/review) |

## Set 1: Blockquoted table

A table inside a blockquote container, testing prefix-aware rendering.

> | Method | Description | Case Study | Outcome |
> | --- | --- | --- | --- |
> | **Suppression by Omission** | The most **potent** form of media control is *not* distortion but **absence** — stories that challenge power are never told | Coverage of the [1999 NATO bombing](https://example.com/nato): media **systematically omitted** civilian casualties | **73% of Americans** supported the intervention |
> | **Framing and Labeling** | Events are presented within **pre-established narratives** that constrain interpretation; *protesters* become ~~rioters~~ | Hugo **Chávez**: labeled an *"authoritarian strongman"* despite winning [**14 of 15 elections**](https://example.com/chavez) | The framing **pre-legitimized** the 2002 coup attempt |
> | **Face-Value Transmission** | Journalists uncritically relay **official statements** without verification, functioning as `stenographers to power` | **Colin Powell**'s [February 2003 UN presentation](https://example.com/powell) on Iraqi `WMDs` | The `$2 trillion` Iraq War; **4,431** US soldiers killed |

## Set 1: Emoji and CJK table

Mixed-width characters: double-width CJK, VS16 emoji sequences,
and accented Latin characters.

| 著作 (Work) | 中文译介 (Chinese) | 日本語 (Japanese) | Impact |
|---|---|---|---|
| ***Democracy*** **《少数人的民主》** | 2009年由*上海译文出版社*翻译出版；被**北京大学**和`复旦大学`列为参考 | **東京大学**の*アメリカ研究センター*で広く引用；`早稲田大学`でも使用 | Cited in **47** Chinese dissertations |
| **Inventing Reality** **《制造现实》** | *社会科学文献出版社*于2012年出版中译本；**清华大学**新闻学院将其与`《制造共识》`比较 | **立命館大学**の*国際関係学部*で使用；`朝日新聞`の元記者が分析を適用 | Chinese translation sold **23,000** copies |
| **Blackshirts and Reds** | 2017年由**中信出版社**翻译；**豆瓣**评分`8.6`（`4,200`条评价） | **ソ連研究**で最も影響力；*北海道大学*スラブ研究センターで書評 | **120,000+** copies sold in China |

## Set 1: Compact narrow table

A small table that should not need wrapping at normal widths.

| Key | Value |
| --- | --- |
| name | Alice |
| age | 34 |
| city | SF |

## Set 1: Seven-column dense table

Many columns with moderate content, exercising the width allocator.

| Country | Year | Leader | Justification | Motive | Corporations | Cost |
| :-- | --: | :-- | :--: | :-- | :-- | --: |
| **Guatemala** | 1954 | Jacobo **Árbenz** | *Communist beachhead* | United Fruit: `550,000 acres` | United Fruit → **Chiquita** | **200,000** killed |
| **Iran** | 1953 | Mohammad **Mosaddegh** | *Communist takeover* | Oil: Britain got `£170M`, Iran got **£37M** | Anglo-Iranian → *BP* | **Shah** installed |
| **Chile** | 1973 | Salvador **Allende** | *Protect democracy* | Anaconda/`Kennecott`: `$1.1B` mines | **ITT**; *Anaconda*; `Kennecott` | `3,065` killed |
| **Indonesia** | 1965 | **Sukarno** | *Prevent communism* | `$5B` minerals, timber, *Strait of Malacca* | **Freeport-McMoRan**; `Goodyear` | **500K–1M** killed |
| **Iraq** | 2003 | **Saddam Hussein** | *WMDs and al-Qaeda* | **112B barrels** oil, 5th largest | **Halliburton**; *Blackwater*; `Bechtel` | **185K–208K** civilians |

## Set 2: Simple wide table

Personnel records for the second division.

| Name | Age | City | Occupation | Notes |
| :-- | --: | :--: | :-- | :-- |
| Frank Torres | 39 | Chicago, Illinois | Backend Developer at Stripe | Maintains their payment queue system |
| Grace Kim | 26 | Seoul, South Korea | UX Researcher and Designer | Master's from Seoul National University |
| Hassan Ali | 48 | Dubai, United Arab Emirates | VP of Engineering at Logistics Corp | Built teams across three continents |
| Ingrid Svensson | 33 | Stockholm, Sweden | DevOps Lead at Spotify | Pioneered their Backstage deployment |
| Juan Morales | 55 | Mexico City, Mexico | Distinguished Engineer at Oracle | 30 patents in database optimization |

## Set 2: Inline markdown table

Additional works with dense inline markup.

| Title | Year | Publisher | Primary Thesis | Recognition |
| :-- | --: | :--: | :-- | :-- |
| **Blackshirts and Reds** | 1997 | *City Lights Books* | The collapse of **Soviet-style socialism** resulted not from internal failure but from sustained **external pressure** — military encirclement, economic warfare, and `covert destabilization` | Sold over **120,000 copies** in China alone; [translated into `12` languages](https://example.com/translations) |
| ***The Assassination of Julius Caesar*** | 2003 | *The New Press* | Roman historians **systematically distorted** the motives of popular leaders; the *populares* represented genuine **class interests** against senatorial oligarchy | Called "a *masterwork* of historical revisionism" by `ClassicalReview.org`; adopted at **85 universities** |
| **God and His Demons** | 2010 | *Prometheus Books* | Organized religion functions as a **political instrument** — providing `ideological cover` for **colonial expansion**, *slavery*, and the suppression of **democratic movements** | [Reviewed in *Free Inquiry*](https://example.com/freeinquiry); **ISBN** `978-1-61614-198-7`; listed by *Humanist Press* |
| **Profit Pathology and Other Indecencies** | 2015 | *Paradigm Publishers* | The **profit motive** penetrates every institution — healthcare, education, media, the military — creating `systemic pathologies` that no *incremental reform* can address | Last published collection; [foreword by **Noam Chomsky**](https://example.com/foreword) |

## Set 2: Blockquoted table

Another blockquoted table with different content.

> | Technique | Mechanism | Historical Example | Effect |
> | --- | --- | --- | --- |
> | **Red-Baiting** | Associating any **progressive reform** with *Soviet communism* to delegitimize it — a technique surviving **decades** past the Cold War | **Martin Luther King Jr.** was labeled a [*communist agent*](https://example.com/mlk) by `FBI COINTELPRO` | The **Poor People's Campaign** was effectively dismantled |
> | **Expert Sourcing** | Media rely on a rotating cast of `think-tank analysts` funded by the same **corporate interests** they ostensibly scrutinize | The [**Heritage Foundation**](https://example.com/heritage) appears in `34%` of *all Sunday show segments* on foreign policy | Public debate is **bounded** within corporate-acceptable limits |
> | **Trivialization** | Reducing **systemic critique** to personal ~~eccentricity~~ or *fringe opinion*, stripping it of analytical credibility | The ***Occupy Wall Street*** movement: covered as *spectacle* rather than as a response to the [2008 financial crisis](https://example.com/2008) | `67%` of coverage focused on **protesters' appearance** |

## Set 2: Emoji and CJK table

Translation and reception in additional Asian markets.

| 著作 (Work) | 한국어 (Korean) | Tiếng Việt (Vietnamese) | Impact |
|---|---|---|---|
| ***Democracy*** **《소수를 위한 민주주의》** | 2011년 *한울아카데미*에서 번역 출판；**서울대학교**와 `고려대학교`에서 교재로 채택 | **Đại học Quốc gia Hà Nội**의 *Khoa Chính trị học*에서 참고 문헌으로 사용；`NXB Tri Thức` 출판 | Cited in **31** Korean dissertations |
| **Inventing Reality** **《현실 만들기》** | *커뮤니케이션북스*에서 2013년 출판；**연세대학교** 언론학과에서 `《여론 조작》`과 비교 연구 | **Học viện Báo chí và Tuyên truyền**에서 사용；`báo Tuổi Trẻ`의 전 기자가 분석 적용 | Korean translation sold **15,000** copies |
| **Blackshirts and Reds** | 2018년 **창비**에서 번역；**알라딘** 평점 `9.1` (`3,800`개 리뷰) | **Nghiên cứu Liên Xô**에서 가장 영향력；*Đại học Đà Nẵng* Trung tâm nghiên cứu 서평 | **85,000+** copies sold in Korea |

## Set 2: Compact narrow table

Another small lookup table.

| Key | Value |
| --- | --- |
| host | 10.0.0.1 |
| port | 5432 |
| db | prod |

## Set 2: Seven-column dense table

More interventions, different decades.

| Country | Year | Leader | Justification | Motive | Corporations | Cost |
| :-- | --: | :-- | :--: | :-- | :-- | --: |
| **Nicaragua** | 1981 | Daniel **Ortega** | *Soviet proxy state* | Coffee, sugar, `banana exports` | **Standard Fruit**; *C&H Sugar* | **30,000** killed |
| **Libya** | 2011 | Muammar **Gaddafi** | *Humanitarian intervention* | **48B barrels** oil; gold dinar threat | *Total SA*; **ENI**; `ConocoPhillips` | **30K–50K** killed |
| **Vietnam** | 1955 | Ho Chi **Minh** | *Domino theory* | Tungsten, tin, `rubber plantations` | **Dow Chemical**; *Monsanto* | **2M–3M** Vietnamese |
| **Congo** | 1961 | Patrice **Lumumba** | *Communist sympathizer* | Cobalt, copper, **uranium** for `$6B` | **Union Minière**; *Société Générale* | **5M** dead (wars) |
| **Afghanistan** | 2001 | **Taliban** regime | *War on Terror* | `TAPI pipeline`; strategic position | **Halliburton**; *DynCorp*; `KBR` | **176K** killed |

## Set 3: Simple wide table

Research team directory with long institution names.

| Name | Age | City | Occupation | Notes |
| :-- | --: | :--: | :-- | :-- |
| Karl Lindqvist | 41 | Gothenburg, Sweden | Principal Research Scientist at Volvo Autonomous | Led the self-driving truck program since 2020 |
| Mei Chen | 29 | Shenzhen, Guangdong Province | AI Compiler Engineer at Huawei | Contributed to MindSpore deep learning framework |
| Olga Petrova | 37 | Saint Petersburg, Russia | Quantum Computing Researcher at ITMO | Published in Nature Physics and Physical Review |
| Raj Patel | 44 | Bangalore, Karnataka, India | Engineering Director at Infosys | Manages a 200-person cloud infrastructure team |
| Sofia Garcia | 50 | Buenos Aires, Argentina | Professor of Computational Linguistics at UBA | Co-authored the Spanish NLP benchmark suite |

## Set 3: Inline markdown table

Third set with different scholarly works and dense formatting.

| Title | Year | Publisher | Primary Thesis | Recognition |
| :-- | --: | :--: | :-- | :-- |
| **The Sword and the Dollar** | 1989 | *St. Martin's Press* | American **military interventionism** is not aberrant but *structural* — the sword protects the dollar, and the dollar funds the sword, in a self-reinforcing cycle of `imperial accumulation` | Described as "the **missing link** between foreign policy and *domestic class structure*" by [*The Nation*](https://example.com/nation) |
| ***Make-Believe Media*** | 1992 | *St. Martin's Press* | Entertainment media manufacture **ideological consent** more effectively than news — through `character archetypes`, narrative framing, and the systematic *absence* of **working-class perspectives** | Used in over `200` media studies courses; [reviewed by **Todd Gitlin**](https://example.com/gitlin) in *Dissent* magazine |
| **America Besieged** | 1998 | *City Lights Books* | The American **right-wing ascendancy** since Reagan constitutes not a *pendulum swing* but a **structural ratchet** — each "centrist" administration `locks in` the previous rightward shift | Part of the City Lights *Open Media* series; **ISBN** `978-0-87286-330-9`; out of print since `2012` |
| ***Contrary Notions*** | 2007 | *City Lights Books* | Collected essays arguing that **mainstream political analysis** operates within boundaries set by `corporate power` — the *thinkable* is defined by what does not threaten **profit structures** | Features `47` essays spanning 1986–2006; [introduced by **S. Brian Willson**](https://example.com/willson) |

## Set 3: Blockquoted table

Blockquoted with longer analytical descriptions.

> | Strategy | Implementation | Case Study | Long-Term Consequence |
> | --- | --- | --- | --- |
> | **Structural Adjustment** | The `IMF` and **World Bank** condition loans on *privatization*, deregulation, and `austerity` — transferring public wealth to **private capital** | **Bolivia**: the [*Water War* of 2000](https://example.com/bolivia) erupted after `Bechtel` privatized Cochabamba's water at IMF insistence | Public water systems in **16 countries** were eventually re-municipalized |
> | **Debt Peonage** | Third World nations are trapped in **perpetual debt service** — paying more in `interest` than they receive in *new loans* — creating a net **capital flow from poor to rich** | **Sub-Saharan Africa** paid [$229B in debt service](https://example.com/africa) between 1980–2004 while receiving `$170B` in new loans | The `HIPC` initiative relieved only **$76B** of the $524B owed |
> | **Currency Destabilization** | Speculative attacks on currencies of nations pursuing **independent economic policy**, aided by `capital account liberalization` demanded by the *Washington Consensus* | The [**1997 Asian Financial Crisis**](https://example.com/asian-crisis): `$100B` fled Thailand, Indonesia, and South Korea in months | IMF "rescue" packages required **fire-sale privatization** of national assets |

## Set 3: Emoji and CJK table

Arabic and Persian translations with mixed-direction text.

| 著作 (Work) | العربية (Arabic) | فارسی (Persian) | Impact |
|---|---|---|---|
| ***Democracy*** **《ديمقراطية القلة》** | 2010 صدرت عن *دار الفارابي* في بيروت؛ اعتمدتها **جامعة القاهرة** و`الجامعة الأمريكية` كمرجع | **دانشگاه تهران** *دانشکده علوم سیاسی* ترجمه و منتشر کرد؛ `انتشارات نی` | Cited in **23** Arabic dissertations |
| **Inventing Reality** **《اختراع الواقع》** | *المركز القومي للترجمة* نشره في 2014؛ **الجزيرة** أشارت إلى تحليله في `12` مقالاً | **دانشگاه شهید بهشتی** روزنامه‌نگاری از آن استفاده کرد؛ `روزنامه شرق` تحلیل را اقتباس کرد | Arabic translation sold **18,000** copies |
| **Blackshirts and Reds** | 2019 ترجمته **دار الساقي**؛ تقييم **Goodreads** العربي `8.3` (`2,100` مراجعة) | **پژوهش شوروی** تأثیرگذارترین اثر؛ *دانشگاه اصفهان* مرکز مطالعات اسلاوی نقد | **65,000+** copies in Arabic markets |

## Set 3: Compact narrow table

Configuration snippet, minimal columns.

| Key | Value |
| --- | --- |
| env | staging |
| region | eu-west-1 |
| ttl | 3600 |

## Set 3: Seven-column dense table

Cold War era and post-9/11 operations.

| Country | Year | Leader | Justification | Motive | Corporations | Cost |
| :-- | --: | :-- | :--: | :-- | :-- | --: |
| **Brazil** | 1964 | João **Goulart** | *Communist drift* | `Iron ore`, soybeans, **$4B** US investment | **Hanna Mining**; *Light SA*; `AMFORP` | **434** killed directly |
| **Greece** | 1967 | Georgios **Papandreou** | *NATO stability* | Strategic Mediterranean `naval bases` | **Litton Industries**; *Esso Pappas* | **8,000** tortured |
| **East Timor** | 1975 | **Fretilin** government | *Anti-communist* | Timor Gap: **5B barrels** oil reserves | *Oceanic Exploration*; **ConocoPhillips** | **183,000** killed |
| **Panama** | 1989 | Manuel **Noriega** | *Drug trafficking* | Canal Zone; **banking sector** `$38B` | *Bechtel*; **United Brands** | `3,000–5,000` killed |
| **Honduras** | 2009 | Manuel **Zelaya** | *Constitutional crisis* | Palm oil, `mining concessions`, **Soto Cano** base | **Chiquita**; *Dole*; `Dinant` | **300+** activists killed |
