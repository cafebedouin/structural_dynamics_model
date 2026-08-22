% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Traces of Buddhist Ground
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This story instantiates the honji suijaku monism reading of the
 *   shinbutsu-shugo kernel: the claim that kami and buddhas are ontologically
 *   identical, kami being phenomenal traces (suijaku) of an underlying
 *   buddha/bodhisattva ground (honji). This is not a description of
 *   shinbutsu-shugo generally but of the specific hierarchical, buddha-prior
 *   theoretical systematization that Shingon and Tendai institutions
 *   developed and that combinatory shrine-temple complexes administered. Two
 *   sibling constraints exist as separate stories: domain_partition (kami and
 *   buddhas as ontologically distinct, governing separate functional domains
 *   of life/purity versus death/impurity) and incoherent_bundle (the claim
 *   that shinbutsu-shugo is not one coherent kernel but an institutionally
 *   sustained bundle of contradictory commitments). Each sibling has its own
 *   epsilon and its own stakeholder set; this story does not average across
 *   them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism: Kami as Traces of Buddhist Ground").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3').
narrative_ontology:cs_kernel_codification('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', formalized).
narrative_ontology:cs_authority_grounding('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', lineage).
narrative_ontology:cs_interpretation_layer_present('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3').
narrative_ontology:cs_reading_relation('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', foundational, buddha_ground_ontologically_prior).
narrative_ontology:cs_axiom_status(buddha_ground_ontologically_prior, holdable).
narrative_ontology:cs_axiom_grounding('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', buddha_ground_ontologically_prior, theological).
narrative_ontology:cs_axiom('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', foundational, kami_lack_independent_ontological_status).
narrative_ontology:cs_axiom_status(kami_lack_independent_ontological_status, holdable).
narrative_ontology:cs_axiom_grounding('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', kami_lack_independent_ontological_status, theological).
narrative_ontology:cs_axiom('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', secondary, systematic_hierarchy_required_for_coherence).
narrative_ontology:cs_axiom_status(systematic_hierarchy_required_for_coherence, holdable).
narrative_ontology:cs_axiom_grounding('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', systematic_hierarchy_required_for_coherence, conventional).
narrative_ontology:cs_reference_frame('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', shingon_tendai_mandala_systematization).
narrative_ontology:cs_drift_state('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', post_meiji_shinbutsu_bunri, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2c07a0bd-ea6c-4419-a500-0fc3a4bb63b3', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_temple_networks).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_theological_scholars).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, combinatory_shrine_temple_complexes).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, independent_kami_cult_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_shrine_priests_without_temple_patronage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, lay_worshippers).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, lay_worshippers).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_ontological_priority).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, single_ultimate_reality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Esoteric Buddhist institutions (Shingon, Tendai) develop and promulgate the honji suijaku theory through mandala systems (e.g., the Ryobu Shinto mandalas) that map specific kami onto specific buddhas/bodhisattvas as their 'original ground.' They control the theoretical apparatus, train the ritual specialists who administer combinatory shrine-temple complexes, and receive patronage and land grants tied to their role as authoritative interpreters of which buddha grounds which kami.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_temple_networks, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_temple_networks, beneficiary).

% Monk-scholars produce the systematizing texts and genealogies that assign hierarchical rank to kami based on their buddha-correspondence. Their intellectual and institutional standing depends on the theory's continued authority; they can move between temple lineages but their expertise has no market outside the honji suijaku framework.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_theological_scholars, beneficiary,
    organized, generational, mobile, national).

% Jingu-ji (shrine-temples) and multiplex religious sites administer combined kami-buddha worship, collecting revenue from pilgrims and patrons who accept the doctrine that visiting the kami shrine is equivalent to venerating its underlying buddha. Their institutional survival is bound to the theory's continued plausibility.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, combinatory_shrine_temple_complexes, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, combinatory_shrine_temple_complexes, agenda_setter).

% Local kami cults with pre-Buddhist genealogies and origin narratives find their kami reassigned a subordinate ontological status - phenomenal trace rather than independent reality - without their participation in the theoretical construction. Their inherited cosmology is absorbed into a hierarchy that places their deity beneath an imported buddha; they lack the textual apparatus or institutional standing to contest the reassignment, and the alternative (refusing incorporation) risks marginalization from the dominant religious economy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, independent_kami_cult_lineages, payer,
    powerless, generational, trapped, local).

% Shrine priests (kannushi) whose sites lack esoteric temple affiliation must either seek incorporation into a jingu-ji arrangement (subordinating their kami's status and often their own authority to visiting or resident monks) or remain outside the dominant patronage networks, losing access to the land grants, pilgrim traffic, and court recognition that flow through combinatory institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_shrine_priests_without_temple_patronage, payer,
    moderate, biographical, constrained, local).

% Ordinary worshippers receive a unified cosmology that resolves apparent contradiction between kami veneration and Buddhist practice, letting them pursue this-worldly benefits (kami) and soteriological aims (buddhas) without perceived conflict. They also inherit the hierarchy's valuation of buddha-grounded practice as ontologically superior, subtly devaluing kami-only worship traditions they may hold from family or regional lineage.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, lay_worshippers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, lay_worshippers, payer).

% Ritualists who maintain kami worship without any Buddhist admixture, and who would argue kami are independently real and not derivative traces of anything, have no institutional platform within the honji suijaku theoretical apparatus to advance that view. Their position surfaces mainly in later Shinto revivalist polemics, centuries after the theory's dominance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kami_only_ritual_specialists, excluded,
    powerless, biographical, trapped, local).

% Modern scholars analyze honji suijaku as a historically specific combinatory theology (shinbutsu-shugo) rather than adjudicating its metaphysical truth, examining how the doctrine served institutional consolidation of Buddhist temple power over the shrine landscape during the Heian and medieval periods.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_temple_networks).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmology that lets communities practice kami veneration and Buddhist devotion within one ritual and institutional system, avoiding open doctrinal conflict between two religious traditions occupying the same physical and social space.
% TRANSFER_FUNCTION: Moves ontological priority, ritual authority, land grants, and pilgrim revenue from independent kami cult lineages and unaffiliated shrine priests toward Buddhist temple networks and the combinatory shrine-temple complexes they administer or supervise.
% ABSENT_VOICES: Kami-only ritual specialists and the original custodians of local kami genealogies are not parties to the theoretical systematization; their traditions are redescribed as partial or phenomenal without their doctrinal input. Later Shinto revivalists (Motoori Norinaga and others) eventually voice this objection, but only after the theory has been dominant for centuries.
% DISAPPEARANCE_RATIONALE: If honji suijaku theory were withdrawn, the legitimating basis for combinatory shrine-temple complexes and Buddhist administrative authority over shrine lands would collapse; the historical record shows exactly this rearrangement occurring during the Meiji-era shinbutsu bunri (forced separation of kami and buddhas), when temple control over shrines was stripped and independent Shinto institutions were reconstituted almost overnight.
% FOUNDING_PROBLEM: Court and aristocratic patrons in the Heian period needed a way to reconcile continued veneration of ancestral and local kami with an increasingly dominant, state-sponsored Buddhist establishment, without requiring the abandonment of either tradition or open theological conflict between them.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji government's 1868 shinbutsu bunri edicts, issued by an outside political authority rather than by either the Buddhist or kami-cult beneficiaries of the arrangement, treated the fusion as an artificial administrative construct to be undone rather than a discovered metaphysical truth; modern historians of religion (outside both the temple networks and the reconstituted State Shinto institutions) similarly read honji suijaku as a historically bounded combinatory theology rather than a live ontological claim.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52: the coordination function is real (it genuinely resolves a live cosmological tension for lay worshippers and prevents open conflict between rival religious establishments), but the theoretical hierarchy consistently assigns buddha-entities ontological priority and kami subordinate status, and this asymmetry channels land, patronage, and ritual authority toward Buddhist institutions. Suppression (0.58) and theater ratio (0.40) both rise over the interval as the doctrine hardens from an initially fluid combinatory practice into a formally systematized, mandala-coded hierarchy (e.g., Ryobu Shinto) whose maintenance increasingly serves institutional legitimation of temple control over shrine lands rather than live cosmological problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   From the temple-network seat, honji suijaku is a theological achievement resolving apparent contradiction between two traditions - a rope. From the independent kami-lineage seat, the same theory operates as an imposed hierarchy that subordinates their deity's ontological standing to justify institutional absorption - a tangled rope shading toward snare. The engine computes this divergence from the declared power/exit structure; the claimed_type (tangled_rope) reflects my judgment that both the coordination function and the asymmetric extraction are structurally real and coexist, which is definitionally the tangled rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Temple networks and their theological scholars sit near the beneficiary end: they author the hierarchy, administer the combinatory institutions, and collect the associated patronage. Independent kami cult lineages and unaffiliated shrine priests sit near the target end: their inherited cosmology is redescribed as ontologically derivative without their participation, and their exit options are trapped or constrained by the dominant religious economy. Lay worshippers are genuinely mixed - real coordination benefit from a unified cosmology, but also quiet absorption of a hierarchy devaluing their kami-only inheritance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling court patronage of kami veneration with rising Buddhist state authority without conflict) is genealogically live only for as long as both religious establishments retain independent political and economic weight. By the medieval period, temple institutions had absorbed enough shrine administration that the 'reconciliation' function was substantially replaced by a rent-collection function - the theory persisted less because cosmological conflict still needed resolving and more because combinatory institutions depended on it for legitimacy. The Meiji shinbutsu bunri separation, imposed by an external political authority rather than negotiated by either religious establishment, is the strongest evidence that the founding problem had gone dead while the arrangement's institutional life continued - a mandatrophy signature this story flags via founding_problem_status: dead against a disappearance_verdict of world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is honji suijaku monism the historically dominant reading of shinbutsu-shugo, or one theoretical current among several coexisting and even contradictory arrangements (domain_partition, incoherent_bundle) that operated simultaneously across different regions, periods, and institutional contexts within Japan?',
    'Comparative textual and institutional history across regions and centuries - do documented shrine-temple complexes uniformly apply hierarchical honji-suijaku mapping, or do many operate on functional domain-partition logic or unsystematized ad hoc combination that resists either monist or partition framing?',
    'If honji suijaku monism was one current among several rather than the dominant structure, this story''s beneficiary/victim assignments describe only the institutions that adopted the strong systematizing theory (chiefly Shingon/Tendai combinatory complexes), not shinbutsu-shugo as a whole - the incoherent_bundle sibling reading may better describe the aggregate historical phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether monism was the dominant or merely one coexisting reading of the shinbutsu-shugo kernel.').

omega_variable(
    buddha_priority_natural_or_constructed,
    'Is the ontological priority assigned to buddhas over kami in this reading a discovered metaphysical truth (as its own theological tradition claims) or a constructed hierarchy that tracked the relative institutional and political power of Buddhist establishments over shrine cults during the Heian and medieval periods?',
    'Trace correlation between the timing/strength of specific honji-suijaku assignments and the relative political fortunes of the temple networks proposing them; a tight correlation supports the constructed reading, while assignments that persisted despite shifts in temple political fortune would support genuine theological conviction independent of institutional interest.',
    'If constructed, the doctrine functions as ideological cover for temple expansion into shrine administration (supporting tangled_rope or even snare framing for the powerless kami-lineage seats); if genuinely theologically motivated and independent of institutional interest, the coordination function is stronger relative to the extraction, and rope framing would be more defensible from the theological-scholar seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(buddha_priority_natural_or_constructed, conceptual, 'Whether the buddha-prior hierarchy reflects theological discovery or institutional power capture.').

omega_variable(
    kami_independent_reality_omega,
    'Do kami possess independent ontological status prior to and apart from any Buddhist framework, such that describing them as ''traces'' of a buddha-ground misdescribes what they are according to their own pre-Buddhist cult traditions?',
    'This is not empirically resolvable from within either tradition''s own framework; it depends on prior commitments about religious ontology that neither historical evidence nor comparative religion methodology can adjudicate from a neutral standpoint.',
    'If kami possess independent status, the monist reading''s classification of kami-only lineages as ''payers'' bearing an imposed subordination is strongly warranted. If honji suijaku correctly describes a real underlying unity, the same lineages are not victims but beneficiaries of a truer cosmology - a genuinely irreducible interpretive fork this story does not attempt to resolve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kami_independent_reality_omega, preference, 'Irreducible ontological ambiguity over whether kami have independent reality apart from any Buddhist ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 150, 0.22).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.3).
narrative_ontology:measurement(kami_tr_t500, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 500, 0.36).
narrative_ontology:measurement(kami_tr_t700, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 700, 0.38).
narrative_ontology:measurement(kami_tr_t850, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 850, 0.39).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1000, 0.4).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.46).
narrative_ontology:measurement(kami_be_t500, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(kami_be_t700, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 700, 0.5).
narrative_ontology:measurement(kami_be_t850, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 850, 0.51).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1000, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 150, 0.44).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(kami_su_t500, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 500, 0.55).
narrative_ontology:measurement(kami_su_t700, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 700, 0.57).
narrative_ontology:measurement(kami_su_t850, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 850, 0.58).
narrative_ontology:measurement(kami_su_t1000, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1000, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel. honji_suijaku_monism (this story) claims single ultimate reality with buddha-priority hierarchy; domain_partition claims ontologically distinct entities governing separate functional domains (life/purity vs. death/impurity) with no hierarchy between them; incoherent_bundle denies the kernel is coherent at all, treating shinbutsu-shugo as an institutionally sustained bundle of contradictory commitments. Each story authors its own epsilon and stakeholder set per the epsilon-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
