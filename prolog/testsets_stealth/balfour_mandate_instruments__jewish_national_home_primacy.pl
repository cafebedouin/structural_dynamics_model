% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Jewish National Home Primacy Reading of the Mandate Instruments (1920-1948)
 *   domain: international law / colonial administration / state formation
 *
 * SUMMARY:
 *   Kernel and reading: this story instantiates ONE reading —
 *   jewish_national_home_primacy — of the contested kernel
 *   balfour_mandate_instruments: the Balfour Declaration (1917) as given
 *   binding effect through the League of Nations Mandate for Palestine
 *   (drafted 1920, confirmed 1922, administered to 1948). The sibling
 *   readings — dual_obligation_indigenous_rights and
 *   mandatory_interpretive_discretion — are separate constraints in separate
 *   files; they are not averaged here and this story does not hedge across
 *   them (epsilon invariance). Under THIS reading the instruments operate as
 *   a directed program of demographic and territorial transformation:
 *   'national home' is construed as a proto-state requiring land access
 *   (Article 6 close settlement), facilitated immigration (Articles 2 and 6),
 *   and Jewish institutional supremacy (Article 4, the Jewish Agency
 *   recognized as a quasi-governmental public body); Arab political
 *   representation is structurally downgraded — repeatedly proposed
 *   legislative councils are rejected whenever their arithmetic would yield
 *   an Arab majority. The arrangement ran 1920-1948 and ended with the
 *   Mandate's dissolution, the declaration of Israeli sovereignty, and the
 *   displacement of most of the Palestinian Arab population. The claim/metric
 *   split is deliberate: the claimed type is tangled_rope — a genuine
 *   coordination function (lawful mass-migration machinery, land-transfer
 *   law, state-capacity construction, an international legal wrapper)
 *   entangled with asymmetric extraction — while the metrics describe the
 *   operation descriptively; the engine computes per-seat classifications
 *   from the structural data. The epsilon referent is the standing
 *   arrangement under contest — the primacy-operated Mandate itself —
 *   assessed by this reading's own lights: the reading does not deny that the
 *   arrangement extracted from the Arab population; it holds that extraction
 *   subordinate to the primary national-home obligation, and epsilon measures
 *   the extraction.
 *
 * KEY AGENTS:
 *   - british_mandatory_administration: agenda setter (institutional/arbitrage) — administers, interprets (White Papers), and enforces the transformation; holds the exit Britain ultimately exercised in 1947-48
 *   - zionist_institutions: primary beneficiary and co-administrator (organized/identity_locked) — Jewish Agency, WZO, JNF; collect land title and quasi-governmental standing
 *   - jewish_immigrants: beneficiary population (moderate/constrained) — arrive under certificates; receive land work and institutional membership
 *   - palestinian_arab_landholders: primary target (moderate/trapped) — lose the territorial base of their political weight under facilitated purchase law
 *   - arab_tenant_farmers: displaced payers (powerless/trapped) — cultivate land sold from under them by absentee owners
 *   - arab_political_leadership: politically downgraded target (organized/trapped) — no legislative channel; exiled or deported during the revolt
 *   - league_of_nations_permanent_mandates_commission: analytical observer (institutional/analytical) — reviews reports and petitions; no enforcement power
 *   - neighboring_arab_governments: excluded voice (organized/constrained) — object from outside the instruments' party structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.78).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.58).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Jewish National Home Primacy Reading of the Mandate Instruments (1920-1948)").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international law / colonial administration / state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '1390cd60-88ae-4831-b558-b152b82a059f').
narrative_ontology:cs_kernel_codification('1390cd60-88ae-4831-b558-b152b82a059f', fixed_text).
narrative_ontology:cs_authority_grounding('1390cd60-88ae-4831-b558-b152b82a059f', lineage).
narrative_ontology:cs_interpretation_layer_present('1390cd60-88ae-4831-b558-b152b82a059f').
narrative_ontology:cs_reading_relation('1390cd60-88ae-4831-b558-b152b82a059f', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('1390cd60-88ae-4831-b558-b152b82a059f', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('1390cd60-88ae-4831-b558-b152b82a059f', foundational, jewish_peoplehood_national_right).
narrative_ontology:cs_axiom_status(jewish_peoplehood_national_right, holdable).
narrative_ontology:cs_axiom_grounding('1390cd60-88ae-4831-b558-b152b82a059f', jewish_peoplehood_national_right, deontological).
narrative_ontology:cs_axiom('1390cd60-88ae-4831-b558-b152b82a059f', foundational, national_home_primary_directive).
narrative_ontology:cs_axiom_status(national_home_primary_directive, holdable).
narrative_ontology:cs_axiom_grounding('1390cd60-88ae-4831-b558-b152b82a059f', national_home_primary_directive, conventional).
narrative_ontology:cs_axiom('1390cd60-88ae-4831-b558-b152b82a059f', secondary, demographic_transformation_legitimate_means).
narrative_ontology:cs_axiom_status(demographic_transformation_legitimate_means, holdable).
narrative_ontology:cs_axiom_grounding('1390cd60-88ae-4831-b558-b152b82a059f', demographic_transformation_legitimate_means, instrumental).
narrative_ontology:cs_reference_frame('1390cd60-88ae-4831-b558-b152b82a059f', national_home_supreme_directive).
narrative_ontology:cs_drift_state('1390cd60-88ae-4831-b558-b152b82a059f', white_paper_1939, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1390cd60-88ae-4831-b558-b152b82a059f', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, arab_tenant_farmers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, arab_political_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, balfour_declaration_commitment).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, jewish_national_home_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, sacred_trust_civilisation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Palestine under a League of Nations grant: issues immigration schedules, enacts land ordinances, maintains security forces, and interprets the founding instruments through periodic policy papers (1922, 1930, 1939). Gains strategic position between Europe and the eastern empire; bears the cost of suppressing the 1936-39 revolt and of holding together two sets of promises its own commissions call irreconcilable. Holds the exit of withdrawal, which it exercised in 1947-48.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% The Jewish Agency (recognized under Article 4 as a public body cooperating with the administration), the World Zionist Organization, and the Jewish National Fund. Co-administer immigration within the certificate system, purchase and hold land in perpetuity for the national project, and build autonomous governing institutions: elected assembly, national council, labor federation, university, armed defense organizations. Their leadership's identity is fused with the national-home enterprise; abandoning it is not a live option for any of them. Land title and quasi-governmental standing flow to these bodies.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter).

% Arrive under labor and immigration certificates, heavily concentrated in the 1932-36 wave. Receive land work under Hebrew-labor hiring preferences, membership in the new institutions, and a legal immigration channel that closed to them almost everywhere else after 1933. Individual options to return to Europe shrink over the interval; collectively they are organized through the institutions that sponsor their arrival.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_immigrants, beneficiary,
    moderate, biographical, constrained, national).

% Hold the land the national institutions purchase. Some large owners, often absentees resident in Beirut, Damascus, or Cairo, sell at rising prices and profit individually; the class as a whole loses the territorial base of its social and political weight as registered title moves. No alternative political framework protects their tenure; the purchase law and land registries are administered in the national home's favor.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, biographical, trapped, regional).

% Cultivate land sold from under them by absentee owners. Protection of Cultivators Ordinances entitle some to compensation or small replacement plots, but eviction is systematic and enforcement of the protections is weak. They have no political channel that represents them and no means to buy the land they work.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, arab_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% The Arab Executive Committee, the Supreme Muslim Council, and the mufti's network. Petition the Permanent Mandates Commission, boycott the 1937 partition proposal, organize the 1936 general strike and the armed revolt that follows. The instruments give them no legislative channel: proposed councils are rejected whenever their arithmetic would yield an Arab majority, and the revolt's suppression exiles or deports much of the leadership.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, arab_political_leadership, payer,
    organized, generational, trapped, national).

% Reviews the administering power's annual reports and hears Arab petitions. Its 1937 deliberations record the Mandate's two obligations as irreconcilable. It can question and record but cannot bind the administering power; its authority is analytical only.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Governments of Egypt, Iraq, Transjordan, Saudi Arabia and the wider Arab world. Object through the Hussein-McMahon correspondence dispute, delegations to London, and the 1939 St. James Conference. They are not parties to the instruments, and their objections register only through the administering power's discretion.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, neighboring_arab_governments, excluded,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a multi-party state-formation project: gives the Zionist settlement enterprise one legal-institutional frame (immigration machinery, land-transfer law, a recognized quasi-governmental Jewish Agency), embeds it in British imperial administration, and wraps both in League-of-Nations international recognition — solving, once and centrally, lawful mass migration, land acquisition at scale, and proto-state capacity building.
% TRANSFER_FUNCTION: Moves land title from Arab owners (disproportionately absentee) to Jewish national institutions; moves political authority from the Arab majority to Jewish institutional bodies; moves demographic composition from Arab majority toward parity and beyond; and moves the labor of enforcement and administration onto the British taxpayer and soldier for the maintenance of the transformation's legal frame.
% ABSENT_VOICES: The Palestinian Arab majority — consent was never solicited for the Balfour Declaration or the Mandate terms; the King-Crane Commission (1919) recorded near-unanimous Arab objection and its report was shelved. Neighboring Arab governments and the wider Arab world object from outside the instruments' party structure. Within the Mandate's own machinery, Arab petitions reach the Permanent Mandates Commission but carry no binding force.
% DISAPPEARANCE_RATIONALE: Remove the instruments' machinery overnight (say 1930): certificate-based immigration stalls, facilitated land registration and purchase law lapses, the Jewish Agency loses its Article 4 standing and its quasi-governmental functions, and the Yishuv's state-in-building loses its international legal wrapper. Land, law, and legitimacy flows all route through the arrangement; the directed transformation does not proceed on the same track, and the 1948 terminal state — sovereignty plus mass Arab displacement — is not reached in the same form.
% FOUNDING_PROBLEM: Give binding international effect to the Balfour Declaration: convert a wartime great-power promise of a 'national home' into an administered legal regime that could lawfully move Jews into Palestine, move land into Jewish institutional hands, and build governing capacity — while nominally preserving the civil and religious rights of the existing population (Article 2).
% FOUNDING_PROBLEM_CORROBORATION: League of Nations Permanent Mandates Commission minutes and annual reports attest the founding problem and its primacy framing from outside the beneficiary set, including the 1937 finding that the twin obligations were irreconcilable; British parliamentary records (Hansard, the White Papers) attest it from the administering seat; the King-Crane Commission report attests the transformation project's scope from a non-mandatory, non-Zionist vantage. Arab petitioners to the Commission attest the same facts while denying the project's legitimacy — corroborating the genealogy, not its warrant.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.45 to 0.78) because all three transfer channels — land title, political authority, demographic composition — move in one direction under the arrangement's machinery, and the machinery's throughput accelerates: the Fifth Aliyah alone (1932-36) moved roughly 190,000 people while JNF holdings and Agency standing grew. Suppression (0.58 at interval end) is the enforcement cost of holding the transformation against Arab resistance: security operations, rejection of legislative-council proposals, exile of leadership. The suppression series is not a smooth ratchet — it spikes at the 1936-39 revolt (0.70) and eases as the revolt breaks — so the scalar reflects the mature enforcement state, not the peak. Theater rises steadily (0.25 to 0.42): Article 2's protective clause and the 'sacred trust' consultation idiom persist as operative language while constraining primacy-serving action ever less often, making the protective component increasingly performative within this reading. Accessibility_collapse (0.55): Arab alternatives — self-governance, land retention, binational arrangements — were progressively closed but never fully; the 1939 White Paper briefly reopened some, and exit-by-emigration existed at high cost. Resistance (0.70): riots in 1920, 1921, and 1929, the 1936 general strike, and the 1936-39 armed revolt — among the most sustained resistance in the interwar imperial system; the arrangement required real coercion to hold. All three series share one time grid (eight points) so every metric is authored at every examined time point. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, inside the engine.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the Zionist institutional seat the arrangement is a coordination structure it co-built: genuine coordination, legitimate under the primary obligation, with identity-locked investment — rope-flavored. From the Arab payer seats the same structure operates as extraction with suppressed exit and downgraded representation — snare-flavored. The British seat is genuinely hybrid: real imperial coordination and strategic benefit entangled with heavy enforcement costs and an unreviewable interpretive burden. The Commission's observer seat sees the hybrid and names the obligations irreconcilable (1937) without power to resolve it. The engine computes this divergence from the power/exit/role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (zionist_institutions, jewish_immigrants) drive their d toward the beneficiary end; identity_locked exit on the institutions stabilizes their low d, since their position is fused with the arrangement itself. The victim declarations (landholders, tenants, leadership) drive d toward the target end; trapped exit pushes all three toward the full-target end, with the powerless, locally bound tenants nearest it. The mandatory administration declares no beneficiary or victim status, so the structural derivation has no data for it and would fall to the canonical fallback; a directionality override places it at d=0.45, slightly on the benefit side of symmetric — imperial strategic gain roughly offset enforcement cost, and its arbitrage-grade exit (withdrawal) keeps it from sitting at the trapped-target end its enforcement burden might otherwise suggest. The override applies to the institutional power atom; the Commission's observer/analytical seat is excluded from directionality by its analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing the national home — was achieved by the interval's end, and the arrangement dissolved with the Mandate in 1948 rather than outliving its function: this is completion, not mandatrophy. The six-questions mismatch consumer will see founding_problem_status=dead against disappearance_verdict=world_rearranges; the cross-check against the theater path (0.42, no piton signature) and the arrangement's actual termination should read that mismatch as achieved-and-dissolved, not captured-and-zombie. Classification as tangled_rope rather than snare matters in both directions: the coordination function was real (mass migration lawfully machined, state capacity built, international recognition secured) — the extraction rode that structure rather than replacing it — and a pure-coordination reading would erase the victims the same structure produced. The classification prevents both mislabels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the jewish_national_home_primacy reading of the balfour_mandate_instruments kernel; how would the sibling readings restructure it?',
    'Read the sibling stories (dual_obligation_indigenous_rights, mandatory_interpretive_discretion) against this one; use the historical adjudication attempts — Churchill White Paper 1922, Passfield 1930, Peel partition proposal 1937, White Paper 1939 — as data on which hierarchy the text can bear.',
    'Under the dual-obligation sibling the priority of beneficiary and victim sets inverts (Arab tenure protection primary) and epsilon is authored from a different normative seat over the same referent; under the discretion sibling the constraint migrates off the instruments entirely onto the interpretive apparatus. Classification is reading-indexed; no averaging across readings is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    primacy_textual_vs_administrative,
    'Does the instruments'' text compel the primacy reading (national home as proto-state directive), or was primacy an administrative construction — British official practice under sustained Zionist leverage — that the same text could have borne otherwise?',
    'Drafting-history philology: the Curzon-Weizmann exchanges over ''national home'' versus ''state'', and the 1922 Churchill clarification''s ''as of right and not on sufferance'' language; plus counterfactual administrative practice under a dual-obligation-prioritizing mandatory.',
    'If primacy is administrative, the extraction is policy-contingent and the snare component is a choice rather than a textual entailment; if textual, the extraction is baked into the kernel itself and the sibling readings are interpretively strained rather than coequal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primacy_textual_vs_administrative, empirical, 'Whether the primacy direction is entailed by the kernel text or constructed by administration.').

omega_variable(
    suppression_endogeneity,
    'How much of the suppressive force on Arab political capacity was externally imposed (British enforcement, representation downgrading) versus endogenous to Arab society (elite land sales to the national institutions, class division between landowning effendis and tenant cultivators)?',
    'Land-transaction records by seller class; analysis of Arab political fragmentation against British administrative design choices, especially the repeated rejection of legislative councils.',
    'If substantially endogenous, the payer class''s effective suppression exceeds what British coercion alone explains, and peasant-elite coalition remedies were structurally foreclosed by the arrangement''s own price incentives; if external, removing the mandatory would have restored Arab political capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_endogeneity, empirical, 'Structural versus endogenous composition of the measured suppression.').

omega_variable(
    instruments_load_bearing,
    'Is the 1948 outcome contingent on the instruments'' facilitation machinery (immigration certificates, land-transfer law, Agency standing), or would autonomous Yishuv state-building have produced comparable transformation absent them?',
    'Counterfactual analysis of Yishuv institutional capacity against instrument-dependent flows: certificate volumes, JNF holdings as a share of the land base, Article 4-derived legal standing in administrative decisions.',
    'If load-bearing, the world_rearranges verdict is confirmed and the reading''s directed-transformation claim is vindicated; if accelerative only, the constraint drifts toward a transitional-accelerant profile with lower structural necessity and the reading overstates the instruments'' role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instruments_load_bearing, empirical, 'Whether the instruments were load-bearing for the outcome or accelerative only.').

omega_variable(
    protective_clause_functionality,
    'Did Article 2''s safeguard of Arab civil and religious rights ever operate as a binding check on primacy-serving action within this reading, or was it purely performative?',
    'Trace Article 2 invocations across administrative decisions and Permanent Mandates Commission reviews; count instances where the clause constrained an immigration, land-transfer, or institutional-supremacy measure.',
    'If purely performative, the theater ratio is understated and the constraint trends toward pure extraction; if occasionally binding, the coordination component is stronger than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_clause_functionality, empirical, 'Functionality of the Article 2 protective clause under the primacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(balf_tr_t0, observed).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(balf_tr_t4, observed).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(balf_tr_t8, observed).
narrative_ontology:measurement(balf_tr_t12, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(balf_tr_t12, observed).
narrative_ontology:measurement(balf_tr_t16, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(balf_tr_t16, observed).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(balf_tr_t20, observed).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(balf_tr_t24, observed).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 28, 0.42).
narrative_ontology:measurement_basis(balf_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(balf_be_t0, observed).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(balf_be_t4, observed).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(balf_be_t8, observed).
narrative_ontology:measurement(balf_be_t12, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(balf_be_t12, observed).
narrative_ontology:measurement(balf_be_t16, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(balf_be_t16, observed).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(balf_be_t20, observed).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 24, 0.75).
narrative_ontology:measurement_basis(balf_be_t24, observed).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 28, 0.78).
narrative_ontology:measurement_basis(balf_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(balf_su_t0, observed).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 4, 0.42).
narrative_ontology:measurement_basis(balf_su_t4, observed).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(balf_su_t8, observed).
narrative_ontology:measurement(balf_su_t12, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(balf_su_t12, observed).
narrative_ontology:measurement(balf_su_t16, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(balf_su_t16, observed).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(balf_su_t20, observed).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(balf_su_t24, observed).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 28, 0.58).
narrative_ontology:measurement_basis(balf_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% Constraint family: the single kernel balfour_mandate_instruments decomposes into three readings with distinct epsilon values, beneficiary structures, and classifications. This primacy story is the upstream operational reading — it is the reading the mandatory actually administered for most of the interval, and its administrative acts (land ordinances, immigration schedules, Agency recognition) are the evidence each sibling reading cites or contests. The dual-obligation sibling reads the same Articles 2 and 6 as primary protections; the discretion sibling relocates the constraint into the interpretive authority itself. Per the epsilon-invariance principle these are separate files linked here; no single story can carry the contest without making epsilon observer-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__jewish_national_home_primacy, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
