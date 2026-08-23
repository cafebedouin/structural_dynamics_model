% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Standing Territorial Arrangement — Palestinian Autochthony Reading
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This story authors ONE reading of the territorial_legitimacy_dual kernel:
 *   the Palestinian autochthony reading, in which legitimate title flows from
 *   continuous habitation, the 1948 displacement constitutes an ongoing
 *   injustice requiring remedy, and the right of return is non-negotiable.
 *   Per the epsilon-referent rule, extractiveness is authored for the
 *   STANDING ARRANGEMENT UNDER CONTEST — the existing sovereignty, residency,
 *   and return-denial regime — assessed by this reading's own lights; the
 *   reading's endorsed remedy is not the referent. The claim and the metrics
 *   are independent authored facts: claimed_type states what this reading
 *   holds structurally true (a hybrid that genuinely coordinates daily life
 *   for all inside it while extracting land, residency, and
 *   self-determination from a defined victim set, held up by active
 *   enforcement), while the metric values describe the arrangement's
 *   operation as this reading observes it. Sibling readings are separate
 *   constraint files linked via network.affects_constraints; nothing about
 *   them is adjudicated here.
 *
 * KEY AGENTS:
 *   - - israeli_state_institutions: agenda-setter (institutional/arbitrage) — administers sovereignty, enforces return denial, collects the arrangement's gains
 *   - - palestinian_refugee_diaspora: primary target (moderate/trapped) — bears multi-generational dispossession; both physical and claim-abandonment exits closed
 *   - - west_bank_gaza_residents: primary target (powerless/trapped) — lives under administered occupation and blockade
 *   - - israeli_settler_population: beneficiary (organized/identity_locked) — collects land and precedence; ideologically fused with presence
 *   - - jewish_diaspora_institutions: beneficiary (organized/arbitrage) — collects the national-home guarantee at zero ground exposure
 *   - - arab_israeli_citizens: dual-positioned payer/beneficiary (moderate/constrained) — inside the arrangement and marked by it
 *   - - lebanese_camp_refugees: excluded voice (powerless/trapped) — would object to compensation-only bargains; not seated
 *   - - un_international_law_bodies: analytical observer (institutional/analytical) — attests the record without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.86).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Standing Territorial Arrangement — Palestinian Autochthony Reading").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '122700ad-4063-4c63-9e82-3607fdc71aeb').
narrative_ontology:cs_kernel_codification('122700ad-4063-4c63-9e82-3607fdc71aeb', distributed).
narrative_ontology:cs_authority_grounding('122700ad-4063-4c63-9e82-3607fdc71aeb', lineage).
narrative_ontology:cs_interpretation_layer_present('122700ad-4063-4c63-9e82-3607fdc71aeb').
narrative_ontology:cs_reading_relation('122700ad-4063-4c63-9e82-3607fdc71aeb', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('122700ad-4063-4c63-9e82-3607fdc71aeb', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('122700ad-4063-4c63-9e82-3607fdc71aeb', foundational, continuous_habitation_confers_primary_title).
narrative_ontology:cs_axiom_status(continuous_habitation_confers_primary_title, holdable).
narrative_ontology:cs_axiom_grounding('122700ad-4063-4c63-9e82-3607fdc71aeb', continuous_habitation_confers_primary_title, deontological).
narrative_ontology:cs_axiom('122700ad-4063-4c63-9e82-3607fdc71aeb', foundational, return_right_non_negotiable).
narrative_ontology:cs_axiom_status(return_right_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('122700ad-4063-4c63-9e82-3607fdc71aeb', return_right_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('122700ad-4063-4c63-9e82-3607fdc71aeb', pre_nakba_habitation_continuity).
narrative_ontology:cs_drift_state('122700ad-4063-4c63-9e82-3607fdc71aeb', contemporary_post_oslo_entrenchment, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('122700ad-4063-4c63-9e82-3607fdc71aeb', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_gaza_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_israeli_citizens).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, fait_accompli_consolidation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers sovereignty over the territory, sets citizenship and property law, operates the permit and closure regimes, and enforces the policy that bars displaced Palestinians from re-entering. Collects the arrangement's gains: territory, security control, and demographic composition. Its exit is reshaping the rules themselves — it can alter the arrangement at will, but doing so carries existential political cost, so it maintains the current configuration instead.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Lives beyond the 1967 line under state protection, receiving land allocations, housing subsidies, dedicated road infrastructure, and military shielding. Core constituencies are bound by religious-national commitments that make relocation unthinkable at any price; a pragmatic minority weighs departure under compensation schemes. Their continued presence is both a product of the arrangement and a load-bearing pillar of it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settler_population, beneficiary,
    organized, generational, identity_locked, regional).

% Philanthropic and advocacy organizations that fund and defend the arrangement from abroad, collecting the assurance of a national home. They can reduce, redirect, or withdraw support at will and bear no direct exposure to conditions on the ground; their distance is precisely what makes their exit cheap.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, jewish_diaspora_institutions, beneficiary,
    organized, biographical, arbitrage, global).

% Descendants of the 1948 displaced, holding refugee registration and the keys and deeds of destroyed villages but barred from entering. Citizenship is absent or nominal in most host states; travel documents are host-issued. Transmission of grievance across generations is the community's central institution. Abandoning the claim is socially and existentially unavailable; exercising it physically is barred by the arrangement itself — both directions of exit are closed.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugee_diaspora, payer,
    moderate, generational, trapped, global).

% Live under military administration, permit-regulated movement, and in Gaza under blockade. Land, water, and movement are allocated by a sovereign they cannot vote out. Physical departure is possible only by emigration, which the community reads as surrender of the collective claim, so staying is compulsory in practice.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, west_bank_gaza_residents, payer,
    powerless, biographical, trapped, regional).

% Hold citizenship, vote, and receive state services, while carrying family histories of expelled relatives and depopulated village sites within the state's own territory. Formal mobility and franchise coexist with exclusion from land-restoration claims and from the state's national narrative — inside the arrangement and marked by it at once.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_israeli_citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, arab_israeli_citizens, beneficiary).

% Concentrated in camps under host-state laws restricting professions and property ownership. Not seated in any negotiating forum; spoken for by factions they did not elect. Would object to any bargain priced in statehood-elsewhere plus compensation, and their absence from the table is what makes such bargains negotiable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, lebanese_camp_refugees, excluded,
    powerless, generational, trapped, regional).

% Pass resolutions, issue advisory opinions, and fund relief attesting the displacement's unresolved status. Takes testimony, compiles the record, and names the obligations it believes are outstanding; holds no enforcement power over any seated party.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, un_international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A sovereign administrative order coordinates law, taxation, infrastructure, utilities, and security across the territory and organizes citizenship and residency for those inside it; parallel bodies (Palestinian Authority municipalities, UNRWA service networks) coordinate segments of the population the sovereign does not administer.
% TRANSFER_FUNCTION: Moves land, property, residency rights, water allocation, and self-determination capacity from displaced Palestinians and residents of the occupied territories toward the Israeli state and its citizen base; moves the custodial burden of the displaced onto host states and international agencies.
% ABSENT_VOICES: Refugee camp populations in Lebanon and Syria, and internally displaced 1948 refugees now inside Israel, are represented indirectly at best — through factional intermediation or not at all. Seated at the table, they would object to any framework that trades the return right for statehood elsewhere or compensation alone; their exclusion is a precondition of the bargains currently on offer.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, sovereignty over the territory would be instantly contested by every party with a registered claim, mass return movements would begin immediately, host states would face sudden repatriation and destabilization, and regional alliances would realign around the vacuum — nearly every arrangement in the region depends on this one holding.
% FOUNDING_PROBLEM: The post-1948 state was built to secure sovereign refuge and self-determination for a persecuted population; the post-1967 control regime was built to administer territory captured in war and manage the security-demographic dilemma of ruling a large non-citizen population. This reading locates the arrangement's persistent defect upstream: the first problem was solved by creating a second, unremedied one.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: UNGA Resolution 194 (III) and successive UNRWA mandates attest the unresolved displacement; UNSC Resolution 242 and the ICJ's 2004 separation-barrier opinion attest that the post-1967 administration has exceeded temporary wartime administration; Amnesty International and Human Rights Watch reporting attests the continuing effects of the displacement. No corroborating source outside the benefiting parties attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86 at interval end) because the arrangement's core transfers — land, property, residency, return — are decoupled from any compensating grant to those they are taken from, and the deprivation compounds across generations. Suppression is higher still (0.88) because persistence depends on actively enforced closure: permit regimes, the separation barrier, the Gaza blockade, and citizenship law that bars return; the mechanism is overwhelmingly structural (physical and legal barriers) rather than internalized, which is why suppression tracks enforcement build-ups so closely. Accessibility_collapse is moderate (0.55): the alternatives — return, compensation, binational confederation — remain articulated and internationally legible, but none is reachable from inside the arrangement, so alternatives persist as discourse while collapsing as options. Resistance is high (0.72): two intifadas, sustained diplomatic campaigns, boycott movements, and armed factions. Theater_ratio (0.42) peaks around negotiation episodes (1993, 2018) when process substitutes for substance. The temporal series run on one shared grid (all three metrics at all eight points). The dynamics are CYCLICAL with a monotonic ratchet underneath: tension builds, an uprising or crisis erupts, crackdown follows, a negotiation episode opens, expectations relax, facts on the ground accumulate, and the cycle repeats — the oscillation itself functions partly as intermittent reinforcement, since each relaxation raises remedy expectations that the next crackdown resets while accumulated facts survive both phases. Base-extractiveness rises monotonically beneath the cycle (facts accumulate in every phase); the end-state metrics were measured at the late-cycle high-tension phase (2024).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the state's position the arrangement is the coordination it provides and defends — law, infrastructure, security, citizenship — and its own experience of the structure is rope-like. From the refugee-diaspora and occupied-resident seats the same structure operates as enforced dispossession with no compensating coordination benefit they can access — snare-like. The arab_israeli_citizens seat computes intermediate: genuine citizenship benefits alongside narrative and property exclusion. Identity-lock dynamics bind the settler seat: the fusion is ideological-religious (presence itself constituted as covenantal duty), so the seat's support for the arrangement survives any price signal; if that identity frame broke, the settler seat would shift toward a mobile beneficiary position and the arrangement's on-ground pillar would weaken sharply. The engine computes these per-seat classifications from the authored structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain runs from beneficiary/victim declarations plus exit options. israeli_state_institutions sits near the full-beneficiary end (d near 0.05): it authors the rules and collects the transfers, and its arbitrage-grade exit (ability to rewrite the rules) damps any residual cost it bears. jewish_diaspora_institutions sit similarly low (d near 0.15) with arbitrage exit. israeli_settler_population derive low d as declared beneficiaries, but their identity_locked exit matters for persistence rather than for their own extraction profile — they are the arrangement's stabilizing constituency, not its target. palestinian_refugee_diaspora sit near the full-target end (d near 0.95): trapped in both directions, generational horizon, the claim itself constituting their identity. west_bank_gaza_residents sit near 0.90: powerless, physically trapped. arab_israeli_citizens derive near-symmetric d (roughly 0.5) from their dual payer/beneficiary declaration — genuine services and franchise against suppressed restitution claims. No directionality overrides were needed: the structural declarations plus exit atoms already produce the correct relationships, and adding overrides would duplicate what the derivation chain owns.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification is what prevents mislabeling in both directions. A pure-extraction reading would erase the real coordination the arrangement performs — it demonstrably organizes law, utilities, and civic life for everyone inside it, including non-Jewish citizens — and would predict collapse that does not occur. A pure-coordination reading would erase the unremedied transfer at the arrangement's core and launder a founding injustice into administrative normality. The tangled_rope claim holds both: genuine coordination function, asymmetric extraction through the same structure, active enforcement required. On the genealogy interview: founding_problem_status is contested and disappearance_verdict is world_rearranges, so the dead-mandate mismatch flag does not fire — the arrangement is not administrating a solved problem while pretending otherwise. But the theater series marks the live risk: theater_ratio spikes at each negotiation peak (0.46 in 1993, 0.47 in 2018), and if process-theater sustains above 0.5 while the founding problem decays further, the piton hypothesis — an arrangement maintained performatively past its function — becomes the live drift candidate. The oslo_architecture_vitality omega routes that question to observable data rather than letting the classification settle it by assumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the territorial_legitimacy_dual kernel; how would the sibling readings (zionist_refuge_reading, two_state_coexistence_reading) classify the same standing arrangement, and where exactly is the disagreement between readings located?',
    'Generate the sibling stories as separate constraint files and compare computed types, epsilon values, and beneficiary/victim sets across the family; the disagreement locus is identified wherever the sibling''s victim set becomes this reading''s beneficiary set.',
    'The zionist refuge reading would invert the beneficiary/victim structure and author low epsilon for the same arrangement; the coexistence reading would author mid-range epsilon with transitional-sunset logic. This file''s classification is valid only within this reading''s seat; cross-reading comparison is the meta-analytic product, not a hedge inside this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: three readings of one legitimacy kernel instantiate three different constraints with different epsilon and victim sets.').

omega_variable(
    valence_of_1948_disagreement,
    'Is the moral valence of the 1948 displacement — ongoing injustice requiring remedy versus legitimate founding under duress — resolvable at all, or is it the irreducible location of the kernel contest?',
    'Not resolvable by evidence alone; resolves only through normative commitment or a negotiated framework both parties accept, which would constitute a new kernel rather than a verdict on this one.',
    'If resolved as ongoing injustice, this reading''s axioms stand and remedy obligations follow structurally; if resolved as legitimate founding, this reading collapses into the sibling reading''s structure and its victim set dissolves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(valence_of_1948_disagreement, conceptual, 'The specific structural element on which the readings diverge: the moral valence assigned to the founding displacement.').

omega_variable(
    return_remedy_scope,
    'Does the non-negotiable return right demand literal physical repatriation of the refugee diaspora, or is it satisfiable by choice-based packages combining return for some with compensation, restitution, and recognition for others?',
    'Negotiated implementation studies, existing refugee preference surveys, and host-state absorption-capacity analysis.',
    'A literal-repatriation-only reading keeps the standing arrangement''s epsilon maximal and the remedy unsatisfiable short of demographic transformation; a package reading makes remedy tractable and would lower the arrangement''s effective extraction once instituted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_remedy_scope, empirical, 'Scope ambiguity inside this reading''s own remedy demand.').

omega_variable(
    refugee_definition_boundary,
    'Who counts as a victim of the displacement — the UNRWA patrilineal-descendant registry numbering in the millions, or the narrower set of surviving expellees and their immediate households?',
    'Comparative audit of the UNRWA registry against archival expulsion and depopulation records.',
    'Victim-set size scales the aggregate extraction attributed to the standing arrangement and materially changes the feasibility calculus of any remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_definition_boundary, empirical, 'Boundary of the victim set under competing registry definitions.').

omega_variable(
    oslo_architecture_vitality,
    'Is the Oslo-era institutional architecture (the Palestinian Authority, Area C provisions, joint coordination committees) functioning coordination or a theatrical shell maintained for process legitimacy?',
    'Track service-delivery outcomes and committee productivity against the theater_ratio series; apply the piton signature test to the sub-architecture.',
    'If the architecture is a shell, the coordination component of the tangled_rope claim erodes and the arrangement drifts toward snare or piton; if functional, the coordination claim holds and the hybrid classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oslo_architecture_vitality, empirical, 'Vitality of the arrangement''s coordination substrate versus its performative maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autochthony_series_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.14).
narrative_ontology:measurement_basis(autochthony_series_tr_t1948, observed).
narrative_ontology:measurement(autochthony_series_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement_basis(autochthony_series_tr_t1967, observed).
narrative_ontology:measurement(autochthony_series_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.31).
narrative_ontology:measurement_basis(autochthony_series_tr_t1987, observed).
narrative_ontology:measurement(autochthony_series_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.46).
narrative_ontology:measurement_basis(autochthony_series_tr_t1993, observed).
narrative_ontology:measurement(autochthony_series_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement_basis(autochthony_series_tr_t2000, observed).
narrative_ontology:measurement(autochthony_series_tr_t2007, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2007, 0.41).
narrative_ontology:measurement_basis(autochthony_series_tr_t2007, observed).
narrative_ontology:measurement(autochthony_series_tr_t2018, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2018, 0.47).
narrative_ontology:measurement_basis(autochthony_series_tr_t2018, observed).
narrative_ontology:measurement(autochthony_series_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(autochthony_series_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(autochthony_series_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.88).
narrative_ontology:measurement_basis(autochthony_series_be_t1948, observed).
narrative_ontology:measurement(autochthony_series_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.83).
narrative_ontology:measurement_basis(autochthony_series_be_t1967, observed).
narrative_ontology:measurement(autochthony_series_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.79).
narrative_ontology:measurement_basis(autochthony_series_be_t1987, observed).
narrative_ontology:measurement(autochthony_series_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.69).
narrative_ontology:measurement_basis(autochthony_series_be_t1993, observed).
narrative_ontology:measurement(autochthony_series_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement_basis(autochthony_series_be_t2000, observed).
narrative_ontology:measurement(autochthony_series_be_t2007, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2007, 0.81).
narrative_ontology:measurement_basis(autochthony_series_be_t2007, observed).
narrative_ontology:measurement(autochthony_series_be_t2018, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2018, 0.84).
narrative_ontology:measurement_basis(autochthony_series_be_t2018, observed).
narrative_ontology:measurement(autochthony_series_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.86).
narrative_ontology:measurement_basis(autochthony_series_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(autochthony_series_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement_basis(autochthony_series_su_t1948, observed).
narrative_ontology:measurement(autochthony_series_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.73).
narrative_ontology:measurement_basis(autochthony_series_su_t1967, observed).
narrative_ontology:measurement(autochthony_series_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.77).
narrative_ontology:measurement_basis(autochthony_series_su_t1987, observed).
narrative_ontology:measurement(autochthony_series_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.63).
narrative_ontology:measurement_basis(autochthony_series_su_t1993, observed).
narrative_ontology:measurement(autochthony_series_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement_basis(autochthony_series_su_t2000, observed).
narrative_ontology:measurement(autochthony_series_su_t2007, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2007, 0.83).
narrative_ontology:measurement_basis(autochthony_series_su_t2007, observed).
narrative_ontology:measurement(autochthony_series_su_t2018, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2018, 0.86).
narrative_ontology:measurement_basis(autochthony_series_su_t2018, observed).
narrative_ontology:measurement(autochthony_series_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(autochthony_series_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'territorial legitimacy in Israel/Palestine' decomposes into three structurally distinct claims — one per reading of the shared kernel. This file (palestinian_autochthony_reading) authors high epsilon with Palestinians as victims and the Israeli state/settler/diaspora complex as beneficiaries. The zionist_refuge_reading sibling authors the same geography with the inverted structure (persecution-history, covenant, and partition acceptance grounding legitimacy; low epsilon from its seat). The two_state_coexistence_reading sibling authors mid-range epsilon with sunset logic (mutual recognition within 1967 lines as a transitional compromise). The upstream/downstream pressure runs from the two maximal readings into the compromise reading: each maximal reading's insistence changes what the compromise framework must contain. All three files link one another via affects_constraints; epsilon differs across the family because the readings are different constraints, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
