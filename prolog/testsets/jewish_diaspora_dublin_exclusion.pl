% ============================================================================
% CONSTRAINT STORY: jewish_diaspora_dublin_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_diaspora_dublin_exclusion, []).

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
 *   constraint_id: jewish_diaspora_dublin_exclusion
 *   human_readable: Jewish Diaspora Exclusion from Dublin Economic and Social Structures (18th-19th Century)
 *   domain: social/economic/political
 *
 * SUMMARY:
 *   From the late 17th through the 18th century, Dublin maintained a
 *   comprehensive exclusionary constraint against its Jewish diaspora
 *   community through layered legal, economic, and institutional mechanisms.
 *   Formal ordinances prohibited Jewish guild membership, property ownership,
 *   occupational licensing in regulated trades, and residential access to
 *   certain zones. These prohibitions were enforced through a combination of
 *   crown authority, guild policing, mob violence, and periodic expulsion
 *   threats. The constraint generated asymmetric extraction: the established
 *   Dublin merchant class consolidated monopoly market access and
 *   price-setting power; the crown extracted special taxes and maintained
 *   leverage over Jewish assets; the broader Christian population secured
 *   religious/cultural conformity and scapegoat displacement. The Jewish
 *   diaspora bore the structural costs: confined occupations (money-lending,
 *   peddling, rag-picking), restricted residential access, vulnerability to
 *   arbitrary enforcement, and permanent legal subordination. Yet by the late
 *   18th century, enforcement had degraded: dead-letter provisions
 *   accumulated, some prohibitions were circumvented through informal
 *   arrangements, and the apparatus persisted more through institutional
 *   inertia than functional extraction. The Enlightenment intellectual
 *   coalition began mobilizing for civil emancipation, reframing the
 *   constraint as irrational rather than inevitable. This constraint
 *   exemplifies how extraction mechanisms can appear natural (religious
 *   difference, cultural boundaries) while operating through contingent
 *   institutional arrangements (guild monopoly, crown patronage systems,
 *   legal ordinance enforcement). The theater ratio's increase over the
 *   interval (0.35 to 0.58) reflects the apparatus's degradation —
 *   enforcement becomes more symbolic, less functionally extractive — even as
 *   suppression remains high due to accumulated legal and social barriers.
 *
 * KEY AGENTS:
 *   - Jewish Diaspora Dublin: Primary victims (powerless/trapped) — confined by legal prohibitions, occupational restrictions, residential barriers, and material dependency on circumventive informal arrangements
 *   - Established Dublin Merchant Guild: Primary beneficiary (institutional/arbitrage) — maintains monopoly market access through exclusion enforcement; coordinates price-setting and trade allocation
 *   - Crown Political Authority: Secondary beneficiary (institutional/arbitrage) — extracts special taxes from Jewish residents; maintains leverage through asset seizure and residence permits; uses inclusion/exclusion as patronage mechanism
 *   - Jewish Merchant Network (Regional): Secondary actor (moderate/constrained) — operates outside Dublin or in exempt categories with higher mobility but continued legal vulnerability; maintains autonomous coordination mechanisms
 *   - Enlightenment Intellectual Coalition: External actor (powerful/mobile) — advocates for civil rights while potentially capturing rent through reform patronage; benefits from access to merchant networks
 *   - Anti-Jewish Ordinance Apparatus: Institutional infrastructure (institutional/constrained) — maintains exclusionary framework but with increasing dead-letter provisions and theatrical enforcement by 18th century
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_diaspora_dublin_exclusion, 0.68).
domain_priors:suppression_score(jewish_diaspora_dublin_exclusion, 0.72).
domain_priors:theater_ratio(jewish_diaspora_dublin_exclusion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_diaspora_dublin_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_diaspora_dublin_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_diaspora_dublin_exclusion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_diaspora_dublin_exclusion, snare).
narrative_ontology:human_readable(jewish_diaspora_dublin_exclusion, "Jewish Diaspora Exclusion from Dublin Economic and Social Structures (18th-19th Century)").
narrative_ontology:topic_domain(jewish_diaspora_dublin_exclusion, "social/economic/political").

domain_priors:requires_active_enforcement(jewish_diaspora_dublin_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_diaspora_dublin_exclusion, established_dublin_merchant_class).
narrative_ontology:constraint_beneficiary(jewish_diaspora_dublin_exclusion, established_dublin_guild_system).
narrative_ontology:constraint_beneficiary(jewish_diaspora_dublin_exclusion, crown_political_authority).
narrative_ontology:constraint_victim(jewish_diaspora_dublin_exclusion, jewish_diaspora_dublin).
narrative_ontology:constraint_victim(jewish_diaspora_dublin_exclusion, economic_participation_access).
narrative_ontology:constraint_victim(jewish_diaspora_dublin_exclusion, civil_participation_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JEWISH DIASPORA DUBLIN (SNARE) — Trapped by legal prohibitions on guild membership, property ownership restrictions, occupational licensing barriers, and residential confinement. Extraction is maximal: the constraint provides no coordination benefit to this agent; it exists purely to channel resources and status toward the beneficiary class. Exit options are material barriers (expulsion threat, legal prohibition of residence, confiscatory taxation), not mere costs. The community experiences this as pure coercion.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: JEWISH MERCHANT NETWORK — REGIONAL PERSPECTIVE (TANGLED ROPE) — Jewish merchants operating outside Dublin or in exempt categories experience the constraint as mixed: they benefit from exclusive access to certain trade routes and credit networks (coordination function), but bear the structural cost of legal uncertainty, mobility barriers between regions, and vulnerability to revocation. They have somewhat higher exit capacity than Dublin-confined Jews (can relocate to other European ports, different Irish regions) but still face significant constraints. This perspective shows both coordination (intercommunal credit networks, information sharing) and asymmetric extraction (legal vulnerability, mobility costs).
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DUBLIN MERCHANT GUILD (ROPE) — Experiences the constraint as pure coordination: monopoly guild membership, exclusive market access, and predictable competition structure. The guild enforces and maintains the exclusion as a coordination mechanism for price-setting, apprenticeship standards, and market allocation. This is their genuine institutional function. No extraction from the guild's perspective — the mechanism solves their coordination problem. Arbitrage options allow guild members to shift between markets without losing status.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: CROWN POLITICAL AUTHORITY (ROPE) — Maintains the exclusion framework as a coordination mechanism for: (1) religious conformity enforcement (Catholic/Protestant control), (2) tax extraction (Jewish residents pay special taxes unavailable to excluded classes), (3) debt settlement (crown can seize Jewish merchant assets), (4) political faction leverage (exclusion/inclusion used as patronage). The crown experiences the mechanism as a functional coordination tool. Exit options include treaty negotiation with other powers or modification of the framework — options available only to institutional actors.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-JEWISH ORDINANCE APPARATUS (PITON) — By the 18th century, formal exclusion laws had accumulated significant dead-letter provisions: property restrictions were unevenly enforced, some Jewish merchants held de facto guild access, occupational prohibitions were circumvented through dual-name arrangements and front operations. The apparatus persists through institutional inertia — the ordinances remain on the books and in enforcement budgets — but the functional extraction mechanism has degraded. Much of the enforcement budget is now theater: ceremonial revivals of exclusion during political crises, symbolic harassment, but inconsistent actual barrier maintenance. Theater ratio (0.58) reflects this degradation.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENLIGHTENMENT INTELLECTUAL COALITION (TANGLED ROPE) — European intellectuals advocating for Jewish civil rights simultaneously benefit from access to Jewish merchant networks (capital, trade information) and bear the structural cost of institutional resistance. They have mobile options (can relocate, can abandon the advocacy), but the constraint still structures their choices. From this perspective, the exclusion apparatus functions as both an extractive barrier (limiting access to markets, information, patronage) and as a coordination mechanism (mobilizing religious and corporate interests around a shared outsider identity). The coalition sees an opportunity for rent capture through legal reform patronage.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a civilizational perspective, exclusion appears to reflect timeless religious and cultural boundaries — Jewish difference is 'natural,' exclusion is 'inevitable,' legal barriers reflect deep-rooted identity conflicts. This naturalizing perspective treats contingent institutional arrangements (guild monopoly, crown patronage, anti-Jewish ordinances) as expressions of immutable social structure. The engine will flag this as a false summit: the accessibility_collapse and resistance metrics will show this is actually a Snare, not a Mountain. The naturalizing frame is the constraint itself working — transforming extractive apparatus into apparent natural law.
constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_diaspora_dublin_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jewish_diaspora_dublin_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_diaspora_dublin_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jewish_diaspora_dublin_exclusion, TR),
    TR >= 0.70.

:- end_tests(jewish_diaspora_dublin_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint generates substantial asymmetric resource flows toward the merchant guild and crown, while removing economic participation options for the Jewish diaspora. The value reflects moderate degradation by the 18th century (dead-letter provisions, circumvention arrangements reducing functional extraction) compared to peak enforcement in early 17th century (estimated 0.75+). Suppression (0.72): High. Multiple overlapping barriers exist: legal prohibitions on occupation and property, guild enforcement mechanisms, crown licensing control, mob violence risk, and residential confinement. Suppression remains structural and enforced, though some workarounds exist. Theater ratio (0.58): Moderate-high. Early enforcement was primarily functional (preventing actual guild entry, seizing actual property); by 18th century, much enforcement became symbolic (threatening expulsion without consistent execution, ceremonial harassment, maintaining ordinances without prosecution). The increase reflects constraint degradation — the apparatus persists through inertia, not function. Claimed type (Snare): Justified by ε ≥ 0.46 (0.68), suppression ≥ 0.60 (0.72), and absence of meaningful coordination benefit to victims. The constraint provides no coordination service to the Jewish diaspora — it exists purely to prevent their participation and direct resources to beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The Dublin merchant guild experiences the constraint as Rope — a coordination mechanism solving the genuine collective action problem of market entry control and price stabilization. From their position, the exclusion is functional and consensual. The crown experiences it as Rope — a coordination tool for tax extraction, religious conformity, and political leverage. The Jewish diaspora experiences it as Snare — pure coercion with no coordination benefit. The regional Jewish merchant network experiences it as Tangled Rope — mixed coordination (internal credit/apprenticeship networks) alongside extraction (legal vulnerability, mobility barriers). The Enlightenment intellectual observer experiences it as Tangled Rope — they benefit from market access (rent capture from reform patronage) while bearing the cost of institutional resistance. The analytical observer risks seeing it as Mountain (natural religious difference, inevitable cultural boundaries), but the structural data reveals this as false naturalization. The perspectival gap is maximal between the guild/crown beneficiaries (Rope) and the diaspora victims (Snare) — the same constraint is simultaneously functional coordination and pure extraction depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationships: (1) Jewish diaspora bears full cost of exclusion with zero benefit and no exit (d ≈ 0.98, trapped + victim); (2) Merchant guild benefits from exclusion monopoly with high exit capacity (d ≈ 0.08, beneficiary + arbitrage); (3) Crown extracts through special taxation and asset leverage (d ≈ 0.12, institutional beneficiary + arbitrage). The sigmoid f(d) amplifies the power differential: d=0.98 yields f(d)≈1.42, d=0.08 yields f(d)≈-0.11. Scope modifier σ(local=0.8) dampens the local constraint compared to if it were framed at regional or national scope — the constraint operates most intensely at the Dublin micro-scale. The regional Jewish merchant network experiences higher d (~0.65) due to constrained rather than trapped exit options and mixed benefit from internal coordination mechanisms. This perspective produces the Tangled Rope classification, revealing that suppression operates differentially across Jewish agents depending on their structural position relative to autonomous coordination capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by identifying the analytical observer's false summit: the naturalization of contingent institutional arrangements as immutable social law. The constraint is not a Mountain. The accessibility_collapse metric would show high variability (people can be admitted to markets, residence barriers can be removed, guild membership can be granted) — not the ≥0.85 required for mountains. The resistance metric would show active opposition and circumvention strategies — not the ≤0.15 required for mountains. The constraint is a Snare: it is actively maintained through institutional enforcement (not naturally emergent), it extracts asymmetrically (not for coordination), and it can be dissolved through legal reform (not logically or physically inevitable). The false summit detector catches the analytical observer's error: treating the constraint as natural law when it is contingent institutional design. The six perspectives reveal this: five out of seven perspectives classify the constraint as some form of Tangled Rope or Snare (mixed or extractive); only the civilizational analytical perspective risks the false mountain classification, and only because it naturalizes what is actually institutional contingency. The constraint's degradation trajectory (theater increasing, extractiveness declining slightly) confirms this diagnosis: if it were a mountain, both metrics would remain constant; the degradation shows it is an artificial maintenance problem, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_efficacy,
    'Are formal legal prohibitions on Jewish occupation, property, and residence actively enforced or maintained as theater by the 18th century?',
    'Historical analysis of enforcement records, prosecution patterns, property disputes, and guild complaint frequency. Comparison of ordinance text against documented compliance rates.',
    'If actively enforced: classification remains Snare (high suppression, high extraction). If theatrical: classification degrades toward Piton (high theater_ratio, reduced functional extraction). Theater ratio (0.58) suggests partial degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_efficacy, empirical, 'Whether legal exclusion is functionally enforced or theater').

omega_variable(
    jewish_merchant_network_autonomy,
    'To what degree do Jewish merchants maintain autonomous credit, apprenticeship, and trade networks independent of Dublin guild structure?',
    'Historical analysis of internal Jewish community economic institutions, informal credit arrangements, occupational specialization patterns, and correspondence networks.',
    'If highly autonomous: Jewish diaspora experiences this as Tangled Rope (has coordination function), not Snare. If subordinated: Snare classification confirmed. Autonomy reduces experienced extraction chi for that perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_merchant_network_autonomy, empirical, 'Degree of autonomous Jewish merchant network organization').

omega_variable(
    crown_extraction_motive,
    'Is crown enforcement of exclusion driven by religious conformity objectives or by revenue extraction from special Jewish taxes and asset seizure?',
    'Analysis of crown revenue records, tax policy changes, and pattern correlation between enforcement intensity and treasury capacity. Examination of policy documents for stated vs. actual objectives.',
    'If religious: constraint exhibits identity_coordination suppression mechanism (enforcement theater disguises religious/cultural identity enforcement). If revenue: constraint exhibits pure economic extraction mechanism (Snare classification confirmed). Likely: mixed, with revenue motive increasing over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_extraction_motive, empirical, 'Whether crown enforces exclusion for religious or fiscal motives').

omega_variable(
    civil_emancipation_timeline,
    'What triggers the transition from Snare (active exclusion) to Scaffold (temporary, sunset provisions) to Rope (inclusive coordination) in the emancipation period?',
    'Historical analysis of reform legislation, institutional capacity shifts, economic incentive changes, and political coalition formation. Identification of inflection points where beneficiary class loses enforcement capacity.',
    'If external shock (industrial economy demands labor): timeline and transition speed differ from internal decay scenario. Classification trajectory informs whether constraint persists due to structural function or institutional inertia. Identifies whether reforms are genuine (Scaffold with real sunset) or theatrical (continued extraction under liberalized language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_emancipation_timeline, empirical, 'Causal drivers of transition from active exclusion to civil emancipation').

omega_variable(
    suppression_internalization,
    'To what extent do Jewish diaspora agents internalize exclusionary framing as legitimate — treating the constraints as natural or deserved?',
    'Analysis of contemporary community documents, rabbinic responsa, petition language, and internal correspondence. Examination of acceptance vs. resistance patterns in legal compliance and adaptation behavior.',
    'If high internalization: effective suppression exceeds structural measures — agents carry the constraint with them and police their own compliance. This transforms suppression from structural (0.72) to internalized (potentially 0.85+), making post-exclusion recovery more difficult. If low internalization: agents resist actively, reducing effective suppression and enabling coalition formation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree to which Jewish diaspora internalizes exclusionary framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_diaspora_dublin_exclusion, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jdd_tr_t0, jewish_diaspora_dublin_exclusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jdd_tr_t25, jewish_diaspora_dublin_exclusion, theater_ratio, 25, 0.42).
narrative_ontology:measurement(jdd_tr_t50, jewish_diaspora_dublin_exclusion, theater_ratio, 50, 0.5).
narrative_ontology:measurement(jdd_tr_t75, jewish_diaspora_dublin_exclusion, theater_ratio, 75, 0.56).
narrative_ontology:measurement(jdd_tr_t100, jewish_diaspora_dublin_exclusion, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(jdd_be_t0, jewish_diaspora_dublin_exclusion, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(jdd_be_t25, jewish_diaspora_dublin_exclusion, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(jdd_be_t50, jewish_diaspora_dublin_exclusion, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(jdd_be_t75, jewish_diaspora_dublin_exclusion, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(jdd_be_t100, jewish_diaspora_dublin_exclusion, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_diaspora_dublin_exclusion, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_diaspora_dublin_exclusion, dublin_guild_monopoly).
narrative_ontology:affects_constraint(jewish_diaspora_dublin_exclusion, irish_catholic_conformity_enforcement).
narrative_ontology:affects_constraint(jewish_diaspora_dublin_exclusion, crown_special_taxation).

% DUAL FORMULATION NOTE:
% The Jewish diaspora exclusion decomposes into at least three structurally distinct constraints: (1) occupational exclusion via guild monopoly (economic extraction, coordination function for guild), (2) religious conformity enforcement via legal prohibition (identity coordination function), (3) crown special taxation via legal residence permits (fiscal extraction function). Each has different ε, different beneficiaries, different mechanisms. This story models the integrated constraint as it was experienced by the diaspora community; decomposed analysis would separate the coordination functions from the purely extractive mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_diaspora_dublin_exclusion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
