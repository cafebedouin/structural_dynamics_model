% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant Under Temporal Accommodation Reading
 *   domain: religious_law/political_theology/commitment_systems
 *
 * SUMMARY:
 *   A religious authority maintains an eternally binding marriage covenant
 *   doctrine while simultaneously suspending its enforcement through temporal
 *   accommodation: members who divorce or remarry may participate in
 *   community life because civil law takes precedence in the temporal realm,
 *   but the covenant remains eternally valid in divine law and may be
 *   reasserted if political constraints lift. This constraint exemplifies a
 *   commitment-system reading of a contested kernel — the eternal covenant is
 *   the fixed text, the temporal accommodation is one reading of how it
 *   relates to state law and member conscience. The reading instantiated here
 *   treats accommodation as temporary (scaffolded), contingent on federal
 *   secular authority, with dormant doctrine pending potential restoration.
 *   Alternative readings (immutable_commandment_reading: covenant is never
 *   suspended, only reinterpreted; prophetic_override_reading: covenant is
 *   superseded by newer revelation) coexist as live positions held by
 *   different theological factions. The temporal accommodation reading
 *   occupies the institutional mainstream — the official doctrine of most
 *   Western Christian churches — and creates a perspectival structure where
 *   the same covenant doctrine appears as natural law (mountain) to the
 *   civilizational observer, as coordination mechanism (rope) to the
 *   authority, as temporary innovation (scaffold) to organized reformers, as
 *   degraded ritual (piton) to custodians, as mixed extraction and
 *   coordination (tangled rope) to divorced members, and as inescapable
 *   binding (snare) to those identity-fused with covenant doctrine.
 *
 * KEY AGENTS:
 *   - Religious Authority Structure: Primary beneficiary (institutional/arbitrage) — maintains doctrinal supremacy by accommodating rather than renouncing, sustains institutional legitimacy through flexibility without conceding authority
 *   - Faithful Bound by Covenant: Primary victim (powerless/identity_locked) — identity fused with covenant permanence; bears moral condemnation for accommodation despite civil law permission
 *   - Divorced or Remarried Members: Secondary victim (moderate/constrained) — experience constraint as mixed: accommodation permits civil remarriage but extracts spiritual standing and community integration costs
 *   - Doctrinal Accommodationists: Organized reformers (organized/constrained) — see accommodation as temporary scaffold solving current political mismatch; expect natural sunset as secularization deepens
 *   - Doctrinal Custodians: Institutional guardians (institutional/arbitrage) — maintain covenant doctrine's eternal form through ritual and education despite operational accommodation; see role as preserving truth in abeyance
 *   - Theological Traditionalists: Secondary beneficiaries (moderate/constrained) — benefit from accommodation's appearance of flexibility while retaining doctrinal supremacy; perceive reading as clever reconciliation rather than concession
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the federal/eternal split as immutable feature of religious law rather than contestable political theology choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.38).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.52).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant Under Temporal Accommodation Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'fe039545-5776-4638-87d5-81bdd0641075').
narrative_ontology:cs_kernel_codification('fe039545-5776-4638-87d5-81bdd0641075', formalized).
narrative_ontology:cs_authority_grounding('fe039545-5776-4638-87d5-81bdd0641075', lineage).
narrative_ontology:cs_interpretation_layer_present('fe039545-5776-4638-87d5-81bdd0641075').
narrative_ontology:cs_reading_relation('fe039545-5776-4638-87d5-81bdd0641075', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('fe039545-5776-4638-87d5-81bdd0641075', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('fe039545-5776-4638-87d5-81bdd0641075', foundational, doctrine_dormancy_preserves_authority).
narrative_ontology:cs_axiom_status(doctrine_dormancy_preserves_authority, holdable).
narrative_ontology:cs_axiom_grounding('fe039545-5776-4638-87d5-81bdd0641075', doctrine_dormancy_preserves_authority, conventional).
narrative_ontology:cs_axiom('fe039545-5776-4638-87d5-81bdd0641075', foundational, federal_law_supersedes_ecclesiastical_enforcement).
narrative_ontology:cs_axiom_status(federal_law_supersedes_ecclesiastical_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('fe039545-5776-4638-87d5-81bdd0641075', federal_law_supersedes_ecclesiastical_enforcement, conventional).
narrative_ontology:cs_reference_frame('fe039545-5776-4638-87d5-81bdd0641075', eternal_covenant_divine_validity).
narrative_ontology:cs_drift_state('fe039545-5776-4638-87d5-81bdd0641075', contemporary_secular_jurisdiction, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('fe039545-5776-4638-87d5-81bdd0641075', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, religious_authority_structure).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, doctrinal_continuity_believers).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, divorced_or_remarried_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, member_conscience_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAITHFUL BOUND BY COVENANT (SNARE) — Members identity-fused with the covenant doctrine cannot exit without self-dissolution. Even as civil law permits divorce, religious identity commits them to covenant permanence. Their violation of temporal accommodation doctrine exposes them to moral condemnation within the community while civil law protects them from legal consequence. The constraint operates through internalized doctrine, not external force — but the binding is complete. They experience the constraint as inescapable moral law despite legal exit availability.
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIVORCED OR REMARRIED MEMBERS (TANGLED ROPE) — Constrained by community relationship costs and spiritual standing within the tradition. Civil law permits exit from marriage; religious law permits temporal accommodation but frames it as a concession to weakness, not a valid path. The constraint both enables (permits exit via accommodation) and extracts (demands justification, creates spiritual liability, reduces institutional standing). Benefits from accommodation doctrine (marriage remains valid in state law); bears extraction cost (moral stigma, reduced spiritual authority).
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS AUTHORITY STRUCTURE (ROPE) — Experiences the temporal accommodation doctrine as a coordination mechanism that solves a collective action problem: federal secular law has made covenant enforcement impossible (no legal marriage penalty for violation), but the authority maintains doctrinal continuity by permitting accommodation while preserving the covenant's validity in principle. The authority gains legitimacy by appearing flexible (accommodating civil law) while retaining ultimate doctrinal authority (covenant remains eternally binding in divine law). This is pure coordination — no extraction from this perspective because the authority's power is enhanced by the mechanism.
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOCTRINAL ACCOMMODATIONISTS (SCAFFOLD) — Organized reformers (theologians, pastoral leaders) see temporal accommodation as a temporary structural innovation with a sunset: as civil secularization progresses, the covenant doctrine's grip on member behavior will naturally weaken, and the accommodation becomes the default position. The scaffold has a built-in exit: when political constraints lift (if theocratic authority returns) or when member identity decoupling completes (if secularization deepens), the accommodation doctrine's function vanishes. Currently constraining members but with a perceived path to dissolution.
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DOCTRINAL CUSTODIANS (PITON) — The institutional role of maintaining eternal covenant doctrine in its pure form persists largely through ritual and education despite the accommodation practice making it inoperative. The doctrine is taught as foundational; the accommodation is presented as temporary concession to secular pressure, not as genuine doctrinal evolution. The custodian role has degraded — it maintains a historical claim rather than a living practice — but persists through institutional inertia because the community has invested centuries in the covenant narrative.
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some disjunction between eternal law and temporal accommodation is a structural necessity in any religious system facing secular political authority. The analytical observer risks seeing this as a universal immutable property: 'Religious law always makes concessions to state law; the gap is inherent to the structure of authority.' However, this reading naturalizes what is actually a contestable political theology choice — whether to maintain doctrinal supremacy or accept state sovereignty. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eternal_marriage_covenant__temporal_accommodation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, TR),
    TR >= 0.70.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The authority gains legitimacy through accommodation (appears flexible, respects civil law sovereignty) while retaining doctrinal authority (covenant remains eternally valid). Members experience varying extraction: those identity-locked bear full cost (maximum extraction); those with organizational exit options (arbitrage to secular legal system) experience lower extraction. The intermediate value reflects the mixed experience — genuine coordination benefit (civil law recognition + community participation possible) alongside genuine extraction cost (spiritual liability for those identity-locked to covenant doctrine, community standing reduction for remarried members). Suppression (0.52): Moderate. Civil law removes external enforcement (no legal marriage penalty), but internal enforcement persists through moral culture, community standing mechanisms, and identity-lock binding. The suppression trajectory shows decline over 80-year interval (0.58 → 0.52) as secular socialization reduces doctrine's grip, though some internalized suppression persists. Theater ratio (0.68): Moderate-high. The authority's simultaneous affirmation of eternal doctrine + permission for accommodation has performative elements — the dual messaging is presented as theological coherence but functions partly to preserve institutional authority while appearing to accommodate secular pressure. Doctrinal education maintains the covenant narrative; pastoral practice accommodates members. The theater has increased over the interval as the gap between doctrine and practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival range. The religious authority sees coordination (rope) — accommodation solves the coordination problem of maintaining doctrinal supremacy while respecting secular law. Doctrinal accommodationists see a temporary solution (scaffold) — expecting natural sunset as secularization deepens or political constraints lift. Custodians see a degraded doctrine (piton) — the covenant persists as eternal teaching but operates as ritual performance rather than lived constraint. Remarried members see mixed structure (tangled rope) — accommodation enables remarriage while extracting spiritual standing. Those identity-locked to covenant permanence see inescapable binding (snare) — they experience the doctrine's force despite civil law permission. The civilizational analytical observer risks mountain classification (naturalizing the eternal/temporal split as inherent to religious law), which the engine identifies as false summit: the split is a contestable political theology reading, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: (1) Religious authority with arbitrage options (can maintain doctrine while accommodating civil law) experiences low d ≈ 0.15 (beneficiary with escape routes), applying sigmoid f(0.15) ≈ -0.01, yielding low or negative effective extraction. (2) Faithful with identity-lock exits experience high d ≈ 0.88 (victims with no structural escape because identity is constituted through covenant), applying sigmoid f(0.88) ≈ 1.25, yielding high effective extraction and snare classification. (3) Divorced members with constrained exits experience moderate d ≈ 0.56 (victims with some agency through civil law + accommodation, but spiritual standing costs remain), applying sigmoid f(0.56) ≈ 0.77, yielding moderate-high extraction and tangled rope classification. (4) Accommodationist reformers with constrained exits but organized power experience lower d ≈ 0.42 (some benefits from organizational visibility + reform momentum, some costs from conservative backlash), applying sigmoid f(0.42) ≈ 0.40, yielding moderate extraction and scaffold classification. The false summit detector will flag the analytical observer's mountain classification as a naturalization of contingent political theology.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy under the temporal accommodation reading. The reading coherently classifies as tangled rope at institutional level: genuine coordination function (accommodation allows community participation + state law recognition) + asymmetric extraction (disproportionate moral and spiritual costs borne by those identity-locked to covenant doctrine). The coordination is real (federal pressure creates genuine collective action problem; accommodation solves it). The extraction is real (those identity-locked experience maximum binding despite doctrine's claimed temporality). The perspectival gap is expected and structurally stable: beneficiaries experience rope, victims experience snare, organized reformers experience scaffold. No conflation of coordination with pure extraction occurs. The false summit (analytical mountain) is detected as a misclassification by the engine, not as a mandatrophy resolution need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_dormancy_vs_invalidation,
    'Is the suspended practice doctrine truly dormant (potentially revivable) or effectively invalidated by the lived accommodation?',
    'Historical analysis: does the authority reassert covenant enforcement when political constraints lift (dormancy confirmed)? Or does extended accommodation become de facto doctrine (invalidation confirmed)? Examine similar historical transitions in other religious traditions (e.g., Talmudic accommodation of Temple absence after 70 CE).',
    'If dormancy: the reading''s fundamental premise holds — eternal doctrine remains valid. If invalidation: the reading has misidentified the constraint as temporary when it is actually a new baseline (reclassifies toward piton or rope). This determines whether the constraint is authentically scaffolded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_dormancy_vs_invalidation, empirical, 'Whether suspended doctrine is dormant or invalidated by extended practice').

omega_variable(
    identity_lock_permanence,
    'Is the identity-lock binding members to covenant doctrine permanent (structural identity fusion) or contingent on socialization (revocable through education)?',
    'Generational cohort analysis: do second-generation members of secular societies show declining identity fusion with covenant doctrine? Can explicit de-fusion education (teaching accommodation as theologically valid, not as concession) measurably reduce the binding? Comparison with historical cohorts where identity-lock did dissolve (e.g., Jewish converts to Christianity reducing Torah observance identity fusion).',
    'If permanent: snare classification is stable over generations. If contingent: the identity-lock dissolves with cultural distance, and the constraint naturally transitions toward rope. This affects whether the threat to member conscience is structural or socialization-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permanence, empirical, 'Whether identity-lock binding is permanent or contingent on socialization').

omega_variable(
    authority_credibility_in_dual_messaging,
    'Does the religious authority''s simultaneous affirmation of eternal doctrine + permission for accommodation undermine its credibility, creating a coherence gap that destabilizes the constraint?',
    'Member surveys on perceived contradiction; analysis of internal theological responses (do theologians provide coherent accounts of dormancy/accommodation coexistence?); historical tracking of institutional authority stability during periods of dual messaging vs. unified messaging.',
    'If credibility erodes: the constraint weakens as members perceive the doctrine as performed rather than believed by the authority itself. If credibility holds: the dual messaging is stable because theologians can articulate it coherently, and the constraint maintains force through interpretive sophistication.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_credibility_in_dual_messaging, empirical, 'Whether dual messaging on eternal doctrine + accommodation maintains institutional credibility').

omega_variable(
    kernel_reading_framing_dependence,
    'Does the temporal accommodation reading depend on a specific framing of when the ''eternal'' covenant applies (divine realm only vs. both divine and temporal)?',
    'Comparative theology: examine how different traditions (Roman Catholicism, Eastern Orthodox, Protestant, Islamic jurisprudence) handle the eternal/temporal split, and whether the temporal accommodation reading presumes a particular metaphysical commitment about covenant ontology.',
    'If framing-dependent: the reading is not universally stable — it requires accepting a specific doctrine of two-realm authority. If framing-independent: the reading can accommodate multiple metaphysical commitments about covenant ontology. This is a conceptual omega indicating whether the reading''s coherence depends on contested presuppositions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_dependence, conceptual, 'Whether temporal accommodation reading depends on framing of eternal vs. temporal covenant realms').

omega_variable(
    federal_pressure_counterfactual,
    'Would the temporal accommodation doctrine exist without federal secular law pressure, or is it necessarily reactive (scaffolded) rather than foundational?',
    'Historical comparison: did the reading emerge before secular legal recognition of divorce and remarriage, or only after? Examine theological traditions that developed accommodation doctrine before facing modern secular authority (Islamic jurisprudence on talaq, medieval Jewish takkanot). If pre-secular origin exists, the reading''s scaffold framing may be incorrect.',
    'If necessarily reactive: the scaffold classification holds (accommodation ends if federal pressure lifts). If foundational: the reading may be misclassifying the constraint as temporary when it is actually a sustained theological position that happens to align with federal law. Reclassifies toward tangled_rope with permanent structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_counterfactual, empirical, 'Whether temporal accommodation is reactive to federal pressure or has foundational precedent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_ta_theater_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(emc_ta_theater_t40, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement(emc_ta_theater_t80, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 80, 0.68).

% Extraction over time
narrative_ontology:measurement(emc_ta_extractiveness_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(emc_ta_extractiveness_t40, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(emc_ta_extractiveness_t80, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(emc_ta_suppression_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(emc_ta_suppression_t40, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(emc_ta_suppression_t80, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 80, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The eternal marriage covenant kernel decomposes into three constraint stories, each instantiating a different reading of how eternal doctrine relates to state law and member conscience. Each reading has its own extraction profile (ε values differ: temporal accommodation ≈ 0.38, immutable commandment ≈ 0.45, prophetic override ≈ 0.25), its own beneficiary/victim structure, and its own classification. The stories are linked via network.affects_constraints to enable contamination analysis: if one reading's authority degrades, how does it affect the others?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
