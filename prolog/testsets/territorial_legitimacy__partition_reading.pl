% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via UN Partition and State Recognition (1948 Borders)
 *   domain: political/legal/territorial
 *
 * SUMMARY:
 *   UN Resolution 181 (1948) partitioned the British Mandate territory into
 *   two states based on international legal authority. This reading claims
 *   legitimacy derives from the UN's authority to partition contested
 *   territory and recognize the resulting states within defined borders. Both
 *   Israeli and Palestinian states are legitimate *within those borders*;
 *   settlements beyond 1967 are illegitimate under this reading's frame. The
 *   constraint is CLAIMED as tangled_rope because it provides genuine
 *   coordination (resolving the partition question through legal procedure)
 *   AND extracts asymmetrically (refugees bear displacement costs,
 *   security-dependent populations bear military costs, settlements must be
 *   abandoned). The foundational axiom distinguishing this reading from
 *   siblings is that international legal authority (UN partition vote)
 *   grounds state legitimacy, rather than continuous inhabitation
 *   (indigenous_continuity_reading) or security necessity
 *   (security_necessity_reading).
 *
 * KEY AGENTS:
 *   - israeli_state_entity: Beneficiary and agenda-setter (set borders, enforce them); uses partition framing to claim legitimacy while maintaining security doctrine expanding beyond partition lines (d ~0.25, beneficiary with enforcement power).
 *   - palestinian_state_entity: Beneficiary but materially powerless; entitled to territory but lacks effective control; (d ~0.75, entitled but constrained).
 *   - palestinian_refugees_diaspora: Primary victim; bear displacement cost as the partition's direct consequence; (d ~1.0, full target).
 *   - israeli_security_dependent_populations: Dual-positioned; benefit from state legitimacy but pay military costs the partition's security framing justifies; (d ~0.65, mixed payer-beneficiary).
 *   - settlements_residents_beyond_1967: Trapped; their presence violates the partition reading's own legitimacy frame while defending it; (d ~0.85, high target but ideologically committed to the beneficiary side).
 *   - UN_security_council: Authority without enforcement; declares legitimacy but cannot compel compliance; (d ~0.5, analytical observer).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.62).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.69).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via UN Partition and State Recognition (1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/legal/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '1c3edff3-6282-4003-916c-f004d5497391').
narrative_ontology:cs_kernel_codification('1c3edff3-6282-4003-916c-f004d5497391', formalized).
narrative_ontology:cs_authority_grounding('1c3edff3-6282-4003-916c-f004d5497391', extraction).
narrative_ontology:cs_interpretation_layer_present('1c3edff3-6282-4003-916c-f004d5497391').
narrative_ontology:cs_reading_relation('1c3edff3-6282-4003-916c-f004d5497391', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c3edff3-6282-4003-916c-f004d5497391', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_axiom('1c3edff3-6282-4003-916c-f004d5497391', foundational, international_legal_partition_authority).
narrative_ontology:cs_axiom_status(international_legal_partition_authority, holdable).
narrative_ontology:cs_axiom_grounding('1c3edff3-6282-4003-916c-f004d5497391', international_legal_partition_authority, conventional).
narrative_ontology:cs_axiom('1c3edff3-6282-4003-916c-f004d5497391', foundational, two_state_solution_structurally_possible).
narrative_ontology:cs_axiom_status(two_state_solution_structurally_possible, holdable).
narrative_ontology:cs_axiom_grounding('1c3edff3-6282-4003-916c-f004d5497391', two_state_solution_structurally_possible, instrumental).
narrative_ontology:cs_reference_frame('1c3edff3-6282-4003-916c-f004d5497391', un_partition_framework).
narrative_ontology:cs_drift_state('1c3edff3-6282-4003-916c-f004d5497391', contemporary_de_facto_control, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c3edff3-6282-4003-916c-f004d5497391', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_entity).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_state_entity).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_security_dependent_populations).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settlements_residents_beyond_1967).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_security_dependent_populations).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, territorial_partition_as_conflict_resolution).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_authority_in_state_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Granted territorial legitimacy and state recognition by UN Resolution 181 within defined borders. Simultaneously uses partition legitimacy to claim statehood while maintaining military control and settlement expansion beyond the partition boundary. Sets the enforcement mechanisms for border security, settlement authorization, and Palestinian territorial restrictions. The partition grants legitimacy for the core state territory while de facto Israeli policy expands control beyond it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_entity, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_entity, agenda_setter).

% Entitled to territorial legitimacy and state recognition under UN Resolution 181 within the partitioned area. Lacks effective material control of the allocated territory and faces both Israeli territorial claims and Palestinian movements that reject the partition entirely. The partition framework gives nominal statehood without the infrastructure or control to exercise it; the beneficiary role is formal rather than functional.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_state_entity, beneficiary,
    institutional, generational, constrained, regional).

% Displaced from their pre-1948 homes as a direct consequence of the partition. The partition framework defines them as a 'refugee question' rather than as primary stakeholders in territorial legitimacy. They bear permanent displacement costs (loss of property, inability to return, statelessness for many) while receiving no compensation or repatriation rights under the partition framework. Trapped in refugee camps or diaspora communities; return is effectively forbidden by both partition doctrine (defined borders exclude them) and Israeli security practice.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees_diaspora, payer,
    powerless, generational, trapped, regional).

% Israeli citizens whose safety and identity are tied to the security apparatus that enforces the partition boundaries and manages the Palestinian population. They benefit from state legitimacy and security, but pay ongoing costs: military conscription, defensive infrastructure, border security operations, and ideological commitment to maintaining the partition as a security line. Exit would mean abandoning both security identity and the state legitimacy framework itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_security_dependent_populations, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_security_dependent_populations, beneficiary).

% Inhabit territory that the partition reading declares illegitimate (beyond 1967 boundaries). Their presence functions as the enforcement mechanism maintaining Israeli territorial control in excess of the partition boundary. Trapped by ideology (belief that the land is historically theirs), infrastructure (established communities with institutions), and security integration (settlements are defended by Israeli military). Ideologically committed to rejecting the partition boundary while materially dependent on Israeli state protection.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settlements_residents_beyond_1967, payer,
    organized, biographical, identity_locked, regional).

% Declared and authorized the partition (Resolution 181); established the international legal framework defining territorial legitimacy. Lacks enforcement capacity over the parties; can recognize states, issue resolutions, and frame legitimacy, but cannot compel implementation or punish violations. Authority concentrates in the declaration; enforcement disperses into state action (Israeli border control, Palestinian quasi-governance). The UN's role is definitional rather than operational.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, universal).

% Palestinian national movements that reject partition entirely, viewing 1948 as Nakba (catastrophe) rather than as state formation. Structurally excluded from the partition framework's legitimacy—their voice was absent from the UN vote, and the partition framework itself delegitimizes their claim to the territory as a unified Palestinian homeland. They remain materially present and actively resist the partition constraint, but cannot do so through the partition framework itself (doing so would concede its legitimacy).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_nationalist_movements, excluded,
    organized, generational, trapped, regional).

% Israeli movements claiming historical or religious title to territory beyond the partition boundary (whole-land Zionism, religious nationalism). Excluded from the formal partition framework (which defines them as illegitimate settlers) but materially embedded in settlement expansion and political pressure. They operate by transgressing the partition boundary while invoking security doctrine or historical rights rather than the partition legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_expansionist_movements, excluded,
    organized, generational, trapped, regional).

% Egypt, Syria, Jordan, Lebanon, and other regional states formally recognize both Israeli statehood (within partition borders) and Palestinian rights to self-determination. They navigate internal contradiction: supporting partition legitimacy while providing refuge or military support to actors who reject it. Their position is structurally contradictory—they cannot simultaneously enforce the partition and support its denial.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, regional_state_actors, observer,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, israeli_state_entity).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the overlapping territorial claims of Zionist Jewish diaspora and Palestinian Arab populations by outsourcing the boundary decision to international legal authority (UN) and partitioning the contested territory into two states. Each group receives territorial legitimacy and state recognition within defined borders, replacing force-based competition with a legal framework for coexistence.
% TRANSFER_FUNCTION: Transfers territorial authority from British Mandate governance to two independent states; transfers legitimacy from pre-partition claims (Ottoman suzerainty, Jewish diaspora connection, Arab sovereignty) to UN-recognized statehood; moves Palestinian Arabs across the partition line (displacement, refugee status) and Israeli Jews into newly recognized state territory. The transfer is asymmetric: Israelis gain full statehood and control; Palestinians gain nominal state status but lack effective control and territory is subdivided and non-contiguous.
% ABSENT_VOICES: Palestinian Arabs living in the territory at the time of partition were not represented in the UN vote that partitioned their homeland; Palestinian nationalism's representatives rejected the partition entirely. Israeli expansionist movements (claiming historical rights beyond partition boundaries) were present in the territory but politically marginalized at the 1948 moment. Both categories of excluded actors have since become primary forces shaping the constraint: Palestinian movements denying partition legitimacy, Israeli movements transgressing partition boundaries.
% DISAPPEARANCE_RATIONALE: If the partition constraint vanished, the territorial legitimacy framework would collapse entirely. Israeli state legitimacy currently derives significantly from partition recognition (UN member status, international law standing); Palestinian state legitimacy is wholly dependent on the partition framework (without it, Palestinians have no internationally recognized claim to territory except through indigenous_continuity doctrine, which is contested). The disappearance would either return the territory to pre-partition status (which no party accepts) or require a completely new legitimacy framework to be negotiated—likely based on force-based de facto control (security_necessity reading becomes the sole operating constraint).
% FOUNDING_PROBLEM: British Mandate over Palestine was ending (1948); the territory was claimed by a Jewish diaspora movement seeking statehood (Zionism) and by an Arab/Palestinian population claiming continuous habitation and anti-colonial self-determination. Both claims were militarily organizing; violence was escalating with no existing authority capable of enforcing a solution. An external mechanism (UN partition) was deployed to impose a boundary on incompatible claims.
% FOUNDING_PROBLEM_CORROBORATION: UN Security Council and international law scholarship attest that the founding problem was real: competing territorial claims, no existing state authority, escalating violence. Palestinian scholars and historians attest the partition did not solve the founding problem but displaced it—the problem was redefined as a refugee management question rather than as a legitimacy question, concealing the claim-resolution problem beneath a humanitarian framing. Israeli security scholars attest the founding problem persists in modified form (Palestinian rejection of partition, security threats). No outside party holds that the founding problem has been solved; scholarly consensus is that partition delayed rather than resolved the underlying dispute.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint extracts displacement costs from refugees and ongoing security costs from populations defending the partition. The extraction is not decoupled from a stated function (unlike a pure snare): the extraction is the cost of coordinating two incompatible territorial claims through legal partition. Suppression is high (0.71) because the constraint's persistence depends on actively suppressing competing legitimacy claims (indigenous_continuity and security_necessity readings) rather than on participant preference. Theater rises substantially over the interval (0.15 to 0.48): early implementation required genuine partition enforcement; later periods show increasing performative activity (ceasefire agreements, peace processes, border demarcations) that preserve the partition's nominal status while de facto Israeli territorial control (settlements, security zones) expands. The measurement series show a pattern: extractiveness rises 1948–1967 as the security costs accumulate; stabilizes after 1967 when the security_necessity reading becomes the de facto operating constraint; theater rises as the partition reading becomes increasingly decorative (nominal Israeli recognition of Palestinian claims within partition borders while de facto expansion beyond them). This is the signature of mandatrophy: the founding coordination problem (partition) is solved; what persists is the extraction machinery the partition enabled (security apparatus, settlement expansion justified by security).
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli state seat (agenda-setter, powerful, arbitrage-capable): partition legitimacy is genuine—it granted statehood, international recognition, and a legal framework for development. Security expansions are read as defensive necessity. From the Palestinian refugee seat (powerless, trapped): the partition was the mechanism of dispossession; legitimacy language is theater masking displacement. From the UN seat (institutional observer): the partition is a successful legal precedent for conflict resolution; the fact that it has not been implemented (Palestinian state remains de jure, not de facto) is a separate enforcement problem. The engine computes these divergences from power × exit × beneficiary/victim data; the authored claim does NOT reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state entity: powerful + institutional authority + arbitrage capability (can choose security doctrine or partition doctrine) + designated beneficiary → d ~0.25 (beneficiary end, though with enforcement responsibilities). Palestinian state entity: institutional power atom but constrained exit (cannot leave the framework without disappearing) + beneficiary designation but victim-like material conditions → d ~0.75 (pulled toward target by the powerlessness of actual implementation). Palestinian refugees: powerless + trapped (cannot return) + victim designation → d ~1.0 (full target). Israeli security populations: moderate power + identity_locked exit (cannot exit security identity without leaving Israeli society) + dual role (beneficiary from state existence, payer for security costs) → d ~0.65 (pulled high by the extraction costs, modulated down by the state benefit). Settlements residents: organized power, identity_locked to ideology → d ~0.85 (high target but ideologically committed to the beneficiary reading). No overrides are needed; the derivation captures the structural asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading instantiates mandatrophy at the interval's end (t=76). Founding problem: two incompatible nationalist claims need legal partition to resolve. Status at t=0–12: partition machinery is functioning—borders are demarcated, states are recognized, coordination problem is solved. Status at t=25–38: Israeli territorial expansion (1967 war, subsequent settlement policy) creates de facto deviation from the partition boundary; the partition reading's legitimacy is invoked to defend the deviation (security necessity). Status at t=50–76: the partition remains the nominal legitimacy frame but the de facto operating constraint is the security_necessity reading (1967 borders plus strategic depth). The partition's coordination function persists only as theater (peace agreements signed and violated, borders acknowledged and transgressed). The extraction function persists (security apparatus, settlement expansion, refugee restrictions) but is now justified by security doctrine, not partition doctrine. The theater_ratio rising from 0.15 to 0.48 marks the constraint's decay from coordination to performance. At t=76, the partition reading is mandatrophic: the founding problem it solved (partition agreement) is no longer the functional constraint; what persists is the extraction machinery it enabled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_consent_absent,
    'Can legitimacy rest on a legal partition (UN vote) when one of the two designated beneficiaries (Palestinian Arabs in the territory) did not consent to or participate in the partition decision?',
    'Philosophical analysis of legitimacy frameworks (social contract vs. positivist vs. indigenous sovereignty); corroboration from Palestinian political theory and international law scholarship on consent requirements for state formation.',
    'If legitimacy requires consent and consent was absent from Palestinian Arabs, the partition reading cannot ground legitimate statehood for the Palestinian entity; the indigenous_continuity reading becomes the more defensible alternative. If legitimacy does not require consent, then the partition reading stands but at the cost of conceding that its legitimacy is paternalistic (the UN decided for the inhabitants).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_consent_absent, conceptual, 'Whether absent-party consent undermines the partition''s legitimacy.').

omega_variable(
    partition_enforcement_failure,
    'Does a legal partition remain legitimate when the international authority that declared it (UN) lacks enforcement capacity and the partition boundaries are unilaterally abandoned by one signatory (Israeli settlement expansion)?',
    'Empirical: observe whether the UN partition''s legitimacy survives de facto Israeli territorial expansion. Conceptual: determine whether legitimacy is separable from enforcement capacity, or whether a declared legitimacy that cannot be enforced becomes purely theatrical.',
    'If legitimacy is enforcement-dependent, the partition reading''s legitimacy erodes as suppression capacity fails (Israel maintains de facto control beyond partition boundaries against UN objection). If legitimacy is independent of enforcement, the partition remains legitimate even as its boundaries are violated—but this creates a contradiction with the targeted victims (Palestinian refugees) and the settlements residents (whose presence violates their own legitimacy frame).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_enforcement_failure, empirical, 'Whether unenforced legitimacy claims retain their force.').

omega_variable(
    coordination_extraction_coupling,
    'Is the extraction (refugee displacement, security costs, settlement ideology) intrinsic to the partition mechanism itself, or is it a separate imposition by Israeli security doctrine?',
    'Counterfactual: if the partition had been implemented without Israeli territorial expansion, would the extraction costs (refugee exclusion, settlement expansion) have been avoided? Historical analysis: Palestinian scholarship on whether the partition framework itself (not just Israeli implementation) mandates Palestinian refugee exclusion.',
    'If the extraction is intrinsic to partition (refugees are always the cost of partition), then the partition reading''s classification as tangled_rope is correct and mandatrophy is the predictable end-state. If the extraction is a subsequent imposition, then the partition reading could have remained a purer coordination mechanism if Israeli security doctrine had not commandeered it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_coupling, conceptual, 'Whether partition-as-coordination necessarily produces refugee displacement.').

omega_variable(
    legitimacy_frame_competition,
    'In periods where the partition reading and the security_necessity reading are both deployed to defend the same boundary (e.g., 1967 expansion justified as both defensive necessity AND within historical Jewish claim territory), which reading is the operative legitimacy frame?',
    'Textual analysis of official Israeli statements and legal arguments; discourse analysis showing which legitimacy frame is deployed in each rhetorical context; examination of UN debates where both readings are invoked.',
    'If the security_necessity reading is operative, then the partition reading is merely decorative and the classification should shift toward piton (theater-ratio evidence supports this). If both readings are simultaneously operative, the constraint is hybrid and the engine should compute a two-reading interference pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_frame_competition, empirical, 'Which legitimacy reading functionally operates when multiple readings claim the same boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t6, territorial_legitimacy__partition_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(terr_tr_t6, observed).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy__partition_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(terr_tr_t12, observed).
narrative_ontology:measurement(terr_tr_t25, territorial_legitimacy__partition_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(terr_tr_t25, observed).
narrative_ontology:measurement(terr_tr_t38, territorial_legitimacy__partition_reading, theater_ratio, 38, 0.42).
narrative_ontology:measurement_basis(terr_tr_t38, observed).
narrative_ontology:measurement(terr_tr_t50, territorial_legitimacy__partition_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(terr_tr_t50, observed).
narrative_ontology:measurement(terr_tr_t62, territorial_legitimacy__partition_reading, theater_ratio, 62, 0.48).
narrative_ontology:measurement_basis(terr_tr_t62, observed).
narrative_ontology:measurement(terr_tr_t76, territorial_legitimacy__partition_reading, theater_ratio, 76, 0.48).
narrative_ontology:measurement_basis(terr_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t6, territorial_legitimacy__partition_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement_basis(terr_be_t6, observed).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy__partition_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement_basis(terr_be_t12, observed).
narrative_ontology:measurement(terr_be_t25, territorial_legitimacy__partition_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(terr_be_t25, observed).
narrative_ontology:measurement(terr_be_t38, territorial_legitimacy__partition_reading, base_extractiveness, 38, 0.62).
narrative_ontology:measurement_basis(terr_be_t38, observed).
narrative_ontology:measurement(terr_be_t50, territorial_legitimacy__partition_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(terr_be_t50, observed).
narrative_ontology:measurement(terr_be_t62, territorial_legitimacy__partition_reading, base_extractiveness, 62, 0.62).
narrative_ontology:measurement_basis(terr_be_t62, observed).
narrative_ontology:measurement(terr_be_t76, territorial_legitimacy__partition_reading, base_extractiveness, 76, 0.62).
narrative_ontology:measurement_basis(terr_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t6, territorial_legitimacy__partition_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(terr_su_t6, observed).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy__partition_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(terr_su_t12, observed).
narrative_ontology:measurement(terr_su_t25, territorial_legitimacy__partition_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(terr_su_t25, observed).
narrative_ontology:measurement(terr_su_t38, territorial_legitimacy__partition_reading, suppression_requirement, 38, 0.68).
narrative_ontology:measurement_basis(terr_su_t38, observed).
narrative_ontology:measurement(terr_su_t50, territorial_legitimacy__partition_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(terr_su_t50, observed).
narrative_ontology:measurement(terr_su_t62, territorial_legitimacy__partition_reading, suppression_requirement, 62, 0.71).
narrative_ontology:measurement_basis(terr_su_t62, observed).
narrative_ontology:measurement(terr_su_t76, territorial_legitimacy__partition_reading, suppression_requirement, 76, 0.71).
narrative_ontology:measurement_basis(terr_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__partition_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel contests three readings: indigenous_continuity_reading (legitimacy via continuous Palestinian habitation, 1948 as Nakba), partition_reading (this constraint: legitimacy via UN legal partition, 1948 borders), security_necessity_reading (legitimacy via Israeli security control, 1967 borders plus strategic depth). Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different classification. The partition_reading is affected by both sibling readings: indigenous_continuity delegitimizes the partition by reframing it as dispossession; security_necessity supersedes the partition by redefining the operative boundary. The partition_reading affects both siblings: it provides the legal framework that indigenous_continuity rejects and that security_necessity transgresses. All three readings are linked via affects_constraints in both directions (not shown here, but bidirectional in the full family).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
