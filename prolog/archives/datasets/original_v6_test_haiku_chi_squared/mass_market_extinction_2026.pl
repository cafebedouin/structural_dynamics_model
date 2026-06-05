% ============================================================================
% CONSTRAINT STORY: mass_market_extinction_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mass_market_extinction_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mass_market_extinction_2026
 *   human_readable: The Mass Market Paperback Sunset
 *   domain: economic/cultural
 *
 * SUMMARY:
 *   The mass-market paperback format, which dominated fiction retail for 70
 *   years (1950-2020), entered terminal decline following ReaderLink's 2025
 *   decision to cease distribution. ReaderLink controlled 80% of U.S.
 *   mass-market distribution through wholesalers (Ingram's wholesale
 *   subsidiary, Baker & Taylor). The cessation of ReaderLink's operations at
 *   end-of-2025 effectively eliminated the only remaining large-scale
 *   infrastructure for mass-market logistics. This constraint tracks the
 *   final institutional collapse of the format and the distributional
 *   consequences for readers, authors, and retail ecosystems. The mass-market
 *   paperback was the last truly mass-accessible fiction format: priced at
 *   $7-9 for a novel-length product, stocked in drug stores, pharmacies,
 *   airports, and gas stations where reading-adjacent customers made impulse
 *   purchases. Its elimination forces readers into higher-priced formats
 *   (hardcover ~$25-28, ebook subscription ~$10-15/month) that are
 *   economically inaccessible to transit-dependent, rural, and low-income
 *   urban populations. The constraint exhibits a strong perspectival gap:
 *   publishers and ebook operators frame the extinction as inevitable
 *   'digital evolution'; readers and used-market ecosystems experience it as
 *   active price-gouging and format elimination; independent bookstores
 *   experience it as mixed — they gain margin on upmarket titles but lose the
 *   foot-traffic economics that sustained them. The theater_ratio (0.48)
 *   reflects moderate performativity: the industry narrative naturalizes the
 *   format's death as technological obsolescence rather than acknowledging it
 *   as a margin optimization decision by a concentrated distribution
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - Low-income mass-market readers: Primary victim (powerless/trapped) — face price escalation and format elimination; no alternatives in affordability range
 *   - Drug store & pharmacy retail ecosystem: Victim (moderate/constrained) — lose impulse-purchase category and associated foot traffic; can pivot but lose relationship value
 *   - Mid-list genre fiction authors: Victim (moderate/constrained) — lose 40-60% of revenue stream; face income collapse and career viability threats
 *   - Used book market ecosystem: Victim (powerless/trapped) — depend on high-volume supply; cannot create new supply; face inventory starvation
 *   - Big Five trade publishers: Primary beneficiary (institutional/arbitrage) — capture margin through format upshifting; coordinate with ebook platforms
 *   - Ebook platform operators: Primary beneficiary (institutional/arbitrage) — eliminate competing format; funnel price-sensitive readers to subscriptions
 *   - Independent bookstore coalition: Mixed (organized/constrained) — gain margin on upmarket titles; lose accessibility function and low-income customer base
 *   - Print publishing industry narrative apparatus: Institutional actor (institutional/arbitrage) — maintains false narrative of technological inevitability (piton function)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mass_market_extinction_2026, 0.58).
domain_priors:suppression_score(mass_market_extinction_2026, 0.65).
domain_priors:theater_ratio(mass_market_extinction_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mass_market_extinction_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(mass_market_extinction_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mass_market_extinction_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mass_market_extinction_2026, snare).
narrative_ontology:human_readable(mass_market_extinction_2026, "The Mass Market Paperback Sunset").
narrative_ontology:topic_domain(mass_market_extinction_2026, "economic/cultural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, big_five_trade_publishers).
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, ebook_platform_operators).
narrative_ontology:constraint_beneficiary(mass_market_extinction_2026, independent_bookstore_operators).
narrative_ontology:constraint_victim(mass_market_extinction_2026, mass_market_readers_low_income).
narrative_ontology:constraint_victim(mass_market_extinction_2026, drug_store_retail_ecosystem).
narrative_ontology:constraint_victim(mass_market_extinction_2026, genre_fiction_authors_mid_list).
narrative_ontology:constraint_victim(mass_market_extinction_2026, used_book_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MASS MARKET READER / LOW-INCOME COHORT (SNARE) — These readers (transit commuters, rural communities, price-sensitive urban populations) cannot exit: alternatives (hardcovers $25-28, ebook subscriptions $10-15/month) are economically inaccessible. The mass-market paperback was the last affordable impulse-purchase fiction format at $7-9. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.78. Trapped, bearing full extraction cost through format elimination and price escalation.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DRUG STORE & PHARMACY RETAIL ECOSYSTEM (SNARE) — Walgreens, CVS, Target book sections, truck stops, and airport newsstands historically relied on mass-market paperback consignment and impulse sales (3-8% of pharmacy/drug store revenue). With ReaderLink exit, these retailers lose a low-friction product category. d≈0.85, f(d)≈1.20, σ=1.0 → χ≈0.69. Constrained exit: retailers can pivot to other product lines, but the paperback customer relationship is severed and switching costs are moderate.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-LIST GENRE FICTION AUTHOR (SNARE) — Romance, mystery, and thriller authors who relied on mass-market paperback sales (40-60% of genre fiction revenue stream 2015-2025) face income collapse. d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.63. Constrained exit: they can migrate to ebook-first and self-publishing, but the traditional advance/royalty structure (funded by paperback sales) is eliminated. Career viability degrades for mid-list authors without significant backlist income.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: USED BOOK MARKET / SECONDARY CIRCULATION (SNARE) — Thrift stores, used bookstores, and resale markets (eBay, Poshmark, Little Free Libraries) depend on high-volume paperback circulation. These are the only truly affordable format for lowest-income readers. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Trapped: the used market cannot create new supply; it only redistributes what was already printed. ReaderLink exit shrinks total supply, collapsing the used-book safety valve.
constraint_indexing:constraint_classification(mass_market_extinction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: BIG FIVE TRADE PUBLISHERS (ROPE) — Penguin Random House, Hachette, Simon & Schuster, HarperCollins, and Macmillan benefit from mass-market sunset: it forces readers upmarket to hardcovers ($25-28) and ebook subscriptions (higher margin, bundled with physical retail partnerships). d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary. Publishers frame the format as 'outdated' and coordinate with ebook platforms on bundled offerings. This is coordination for their constituency; extraction for readers.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EBOOK PLATFORM OPERATORS (ROPE) — Amazon Kindle, Apple Books, Google Play, scribd, and Everand benefit directly: mass-market extinction removes a competing affordable format and funnels price-sensitive readers toward subscription models. d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.11. Net beneficiary. Operators coordinate with publishers on DRM, bundling, and recommendation algorithms. They also coordinate with independent bookstores (B&N, Powell's, independent chains) on API and platform features.
constraint_indexing:constraint_classification(mass_market_extinction_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INDEPENDENT BOOKSTORE COALITION (TANGLED ROPE) — Indie bookstores (American Booksellers Association, Indigo in Canada) see both coordination and extraction. Coordination: they move upmarket with publishers through special orders, events, and curation. Extraction: they lose the high-volume, low-margin paperback traffic that sustained foot traffic and incidental purchases. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Organized agents see a mixed picture: survival through curation and community, but reduced accessibility for their lowest-income customer cohort.
constraint_indexing:constraint_classification(mass_market_extinction_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: PRINT PUBLISHING NOSTALGIA NARRATIVE (PITON) — The publishing industry's public framing of mass-market sunset as 'evolution toward digital' is largely theatrical. The real mechanism is margin extraction, not format obsolescence — readers didn't reject paperbacks; they were priced out. theater_ratio=0.48 is borderline, but the narrative function (framing extraction as progress) qualifies this as piton-adjacent. The industry maintains the false narrative that mass-market was always a temporary format, naturalizing what is actually an active elimination decision.
constraint_indexing:constraint_classification(mass_market_extinction_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / FALSE MOUNTAIN (MOUNTAIN) — From a market evolution standpoint, one could argue the mass-market paperback is a natural casualty of digital disruption — print distribution is inherently more expensive than ebooks, so decline is inevitable. However, structural data (ε=0.58, suppression=0.65) contradicts this: the constraint is NOT an immutable property of markets but a choice by ReaderLink and the Big Five to optimize for margin over accessibility. This is a false summit masking an active extraction mechanism. The analytical observer risks naturalizing what is actually a distributional policy.
constraint_indexing:constraint_classification(mass_market_extinction_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mass_market_extinction_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mass_market_extinction_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mass_market_extinction_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mass_market_extinction_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mass_market_extinction_2026, TR),
    TR >= 0.70.

:- end_tests(mass_market_extinction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through two mechanisms: (1) format elimination removes the lowest-priced option, forcing readers upmarket, and (2) concentration of distribution (ReaderLink's near-monopoly on logistics) enables coordinated withdrawal. The extraction is not total (readers have ebook alternatives at higher cost) but substantial for low-income cohorts. The trajectory shows accumulation: 2015-2020 ε≈0.32 (decline phase), 2020-2025 ε≈0.45 (acceleration phase), 2025-2026 ε≈0.58 (collapse phase). Suppression (0.65): Moderate-high. Multiple barriers prevent reader exit or resistance: (1) no organized consumer voice to advocate alternative distribution, (2) platform/publisher coordination blocks independent mass-market revival, (3) used market dependent on historical supply (cannot substitute), (4) library access is underfunded and capacity-constrained, (5) ebook DRM and subscription lock-in raise switching costs. Theater ratio (0.48): Moderate. The industry narrative (framing extinction as technological inevitability) has performative content but also some functional basis — digital distribution IS cheaper per unit. However, margin optimization (pricing readers out) is framed as progress rather than extraction. The theater has declined from 0.35 (2015, when digital was genuinely disruptive novelty) to 0.48 (2026, when the narrative is now recognized as cover for margin extraction). Still below piton threshold (0.70).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a deep perspectival divide. Publishers and ebook operators classify it as Rope (coordination of format evolution) — they see their own actions as solving the problem of print distribution inefficiency. Independent bookstores classify it as Tangled Rope (coordination with margins, but exclusion of low-income customer base). Readers, authors, and retail ecosystems experience it as Snare (pure extraction through format elimination and price forcing). The analytical observer risks the false mountain (naturalizing margin optimization as technological inevitability). The perspectival gap reflects a genuine divergence in what 'the problem' is: publishers see inefficient distribution; readers see pricing out of affordable fiction. No single perspective is false — the constraint legitimately appears different from inside institutional decision-making vs. outside in the market that experiences the consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income readers: Victim + trapped → d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.78. Maximum extraction. No exit options; format elimination is experienced as direct price forcing. Drug store/pharmacy ecosystem: Victim + constrained → d≈0.85, f(d)≈1.20, σ=1.0 → χ≈0.69. High extraction. Can pivot to other products but lose key relationship (impulse purchase, foot traffic). Mid-list authors: Victim + constrained → d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.63. Moderate-high extraction. Revenue collapse but can migrate to ebook-first; career viability threatened but not eliminated. Used book market: Victim + trapped → d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Maximum extraction. No ability to generate new supply; entirely dependent on historical mass-market inventory. Big Five publishers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary. Extraction from readers becomes coordination benefit for publishers. Ebook operators: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.11. Net beneficiary. Eliminate competing format through platform leverage. Independent bookstores: Mixed + constrained → d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Mixed outcome. Gain from upmarket sales; lose from low-income customer base contraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false pure-extraction classification (misidentifying coordination as snare) through the perspectival structure. Publishers DO experience genuine coordination benefits — format consolidation reduces their distribution costs and simplifies inventory management. However, this coordination is ASYMMETRICALLY PAIRED with extraction from readers. The Tangled Rope perspective (independent bookstores) captures this hybridity: the constraint both coordinates publishing operations AND extracts from low-income reading access. The Snare perspective (readers, used market) captures the constraint's real asymmetry: coordination for the beneficiaries, pure extraction for the victims. Mandate resolved: this is NOT a pure snare (which would require zero coordination function); it IS a snare TO THE READERS while being rope TO THE PUBLISHERS, which is exactly the inter-perspectival structure that identifies the constraint as genuinely asymmetric. The false mountain (technological inevitability) is caught by the contradiction between ε=0.58 (extractive) and the 'law of nature' framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    readerlink_return_probability,
    'Will ReaderLink or a successor distributor restart mass-market paperback operations within 10 years given consumer demand or policy intervention?',
    'Market monitoring for new distributor entry; analysis of return-on-investment for mass-market revival; legislative pressure (antitrust, accessibility mandates); consumer demand signals (used market inflation, bootleg printing)',
    'If high probability: constraint is temporary (Scaffold reclassification). If low: constraint is structural (Snare confirmed). If intervention-driven: constraint becomes tangled_rope (policy enforces alternative distribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readerlink_return_probability, empirical, 'Whether ReaderLink or successor will restart mass-market distribution').

omega_variable(
    low_income_reader_substitution_rate,
    'What fraction of mass-market readers migrate to ebook subscriptions vs. exit reading altogether vs. depend on used/library access?',
    'Surveys of low-income reader cohorts; circulation data from public libraries; ebook subscription adoption rates; used bookstore sales tracking',
    'If >80% migrate to paid ebook subscriptions: extraction is moderate (readers bear cost but access preserved). If <50%: readers exit market entirely (extraction is severe, cultural literacy degrades). If dependency on library/used is high: constraint is mitigated by institutions (shifts burden to public sector).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_income_reader_substitution_rate, empirical, 'Reader migration patterns post-mass-market extinction').

omega_variable(
    genre_fiction_revenue_collapse_permanence,
    'Are romance, mystery, and thriller author mid-list incomes permanently depressed post-extinction, or do ebook-first models restore equivalent revenue?',
    'Author income surveys (SFWA, RWA); advance/royalty tracking; indie publishing adoption rates; backlist revenue models',
    'If permanent depression: mid-list author career sustainability is threatened (structural snare for authors). If ebook-first models restore income: constraint is temporary (Scaffold). If trad-pub adapts with advance restructuring: constraint becomes tangled_rope (publishers maintain leverage, authors adapt).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genre_fiction_revenue_collapse_permanence, empirical, 'Whether author mid-list income recovery occurs post-extinction').

omega_variable(
    accessibility_mandate_likelihood,
    'Will governments (US, EU, UK) impose accessibility mandates or subsidies to preserve low-income reading access?',
    'Legislative monitoring; public library funding trends; postal subsidy advocacy; cultural policy development',
    'If mandate implemented: constraint reclassifies to tangled_rope (enforcement mechanism replaces market). If subsidies fund library expansion: Rope (coordination). If no intervention: Snare persists (extraction is primary outcome).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accessibility_mandate_likelihood, preference, 'Whether policy intervention mitigates reading access inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mass_market_extinction_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mmp_tr_t0, mass_market_extinction_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mmp_tr_t5, mass_market_extinction_2026, theater_ratio, 5, 0.42).
narrative_ontology:measurement(mmp_tr_t10, mass_market_extinction_2026, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(mmp_be_t0, mass_market_extinction_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mmp_be_t5, mass_market_extinction_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mmp_be_t10, mass_market_extinction_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mass_market_extinction_2026, resource_allocation).
narrative_ontology:affects_constraint(mass_market_extinction_2026, ebook_drm_enclosure).
narrative_ontology:affects_constraint(mass_market_extinction_2026, publishing_consolidation_market_power).
narrative_ontology:affects_constraint(mass_market_extinction_2026, public_library_funding_constraints).

% DUAL FORMULATION NOTE:
% The mass-market extinction is the terminal outcome of a 15-year constraint sequence: (1) Amazon Kindle ebook disruption (2007-2015, initially Rope for publishers as digital margins rose), (2) publishing consolidation (2013-2020, shifting to Tangled Rope as DRM and bundling replaced open distribution), (3) ReaderLink structural weakness (2020-2025, constraint begins to collapse), (4) ReaderLink exit decision (2025-2026, terminal snare for readers). Each upstream constraint weakened print distribution logistics and reader choice, making the final elimination possible. The extinction is downstream of all three upstream constraints and upstream of the cultural literacy and used-market supply constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mass_market_extinction_2026, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
