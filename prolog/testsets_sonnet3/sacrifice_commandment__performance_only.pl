% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Performance-Only Reading of the Sacrifice Commandment (Suspended, Not Fulfilled, Absent the Temple)
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the
 *   sacrifice-commandment kernel: the rabbinic position holding that the
 *   commandment to bring sacrifices requires physical execution at a standing
 *   Temple altar, and that in the Temple's absence the commandment is
 *   suspended (ones — circumstantially prevented) rather than fulfilled by
 *   any substitute activity, including study. Under this reading, 1,900 years
 *   of intensive scholarly engagement with the laws of Kodashim and Zevachim
 *   — some of the most demanding material in the entire rabbinic corpus —
 *   does not itself satisfy the divine obligation; it is preparatory,
 *   academic, or devotional, but categorically short of performance. The
 *   extractiveness this reading authors is high: it directs enormous,
 *   sustained scholarly labor and religious yearning toward an act that is
 *   structurally guaranteed to remain unperformed for as long as there is no
 *   Temple, while denying that labor's own study any claim to fulfilling the
 *   underlying mitzvah. This is a distinct constraint from the
 *   study_as_performance reading (which claims ε is low or near-zero, since
 *   study itself counts) and from the archive_maintenance reading (which
 *   reframes the same study as inventory-keeping for future use rather than
 *   either performance or worship) — the three readings are not the same
 *   constraint measured three ways; they are three constraints sharing one
 *   kernel text and diverging sharply on where the extraction, if any, falls.
 *
 * KEY AGENTS:
 *   - messianic_restorationist_authorities: institutional beneficiary/agenda_setter (institutional/arbitrage) — administers the suspension doctrine, gains legitimacy from unresolved anticipation
 *   - temple_reconstruction_institutions: organized beneficiary (organized/mobile) — mission depends on physical performance remaining the exclusive criterion
 *   - diaspora_study_communities: payer (moderate/constrained) — labor is doctrinally denied fulfillment-status
 *   - lay_practitioners_seeking_fulfillment: payer (powerless/trapped) — bear permanent, individually inescapable incompletion
 *   - study_as_performance_proponents: excluded (moderate/constrained) — alternative reading marginalized in performance-only institutions
 *   - halakhic_historians: analytical observer — traces the doctrine's institutional hardening over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.71).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.58).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Performance-Only Reading of the Sacrifice Commandment (Suspended, Not Fulfilled, Absent the Temple)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious_studies/halakhic_theory").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '4296d916-f7ab-4104-b40b-74568f4d7821').
narrative_ontology:cs_kernel_codification('4296d916-f7ab-4104-b40b-74568f4d7821', fixed_text).
narrative_ontology:cs_authority_grounding('4296d916-f7ab-4104-b40b-74568f4d7821', lineage).
narrative_ontology:cs_interpretation_layer_present('4296d916-f7ab-4104-b40b-74568f4d7821').
narrative_ontology:cs_reading_relation('4296d916-f7ab-4104-b40b-74568f4d7821', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('4296d916-f7ab-4104-b40b-74568f4d7821', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('4296d916-f7ab-4104-b40b-74568f4d7821', foundational, physical_execution_is_the_sole_fulfillment_criterion).
narrative_ontology:cs_axiom_status(physical_execution_is_the_sole_fulfillment_criterion, holdable).
narrative_ontology:cs_axiom_grounding('4296d916-f7ab-4104-b40b-74568f4d7821', physical_execution_is_the_sole_fulfillment_criterion, conventional).
narrative_ontology:cs_axiom('4296d916-f7ab-4104-b40b-74568f4d7821', foundational, study_of_law_cannot_substitute_for_the_commanded_act).
narrative_ontology:cs_axiom_status(study_of_law_cannot_substitute_for_the_commanded_act, holdable).
narrative_ontology:cs_axiom_grounding('4296d916-f7ab-4104-b40b-74568f4d7821', study_of_law_cannot_substitute_for_the_commanded_act, deontological).
narrative_ontology:cs_reference_frame('4296d916-f7ab-4104-b40b-74568f4d7821', second_temple_sacrificial_performance).
narrative_ontology:cs_drift_state('4296d916-f7ab-4104-b40b-74568f4d7821', post_destruction_rabbinic_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4296d916-f7ab-4104-b40b-74568f4d7821', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, messianic_restorationist_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_reconstruction_institutions).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, diaspora_study_communities).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, lay_practitioners_seeking_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit the ruling that the sacrifice commandment remains suspended, not fulfilled, absent a standing Temple and altar. They administer the doctrinal boundary — determining what counts as performance versus preparation — and their institutional authority is bound up with maintaining anticipation of restoration. They face no cost from the suspension continuing indefinitely; their position is reinforced by every year the commandment stays unperformed and awaited.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, messianic_restorationist_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, messianic_restorationist_authorities, beneficiary).

% Organizations dedicated to preparing implements, training priestly lineages, and lobbying for eventual reconstruction draw legitimacy, funding, and purpose directly from the performance-only reading: if study alone fulfilled the commandment, their preparatory mission would lose its exclusive claim to being the thing that matters. They benefit from the doctrinal insistence that only physical execution counts.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_reconstruction_institutions, beneficiary,
    organized, civilizational, mobile, national).

% Yeshivot and study circles devote enormous scholarly labor to the minutiae of sacrificial law (Kodashim, Zevachim, Menachot) under a doctrinal frame that tells them this labor is preparatory or academic, not itself the fulfillment of the commandment. Their study cannot count as performance under this reading, however rigorous, leaving them formally short of the mitzvah regardless of effort. Exit would mean abandoning traditional curriculum structure or adopting the study_as_performance reading, which their own institutions often resist.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, diaspora_study_communities, payer,
    moderate, generational, constrained, global).

% Individuals who wish to fulfill the full complement of commandments in their lifetime encounter, under this reading, an entire category of mitzvot rendered categorically unfulfillable through any available action. They bear the psychological and religious cost of permanent incompletion with no exit — they cannot build a Temple, and study does not substitute. Their situation is structurally identical across generations with no individual recourse.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, lay_practitioners_seeking_fulfillment, payer,
    powerless, biographical, trapped, local).

% Rabbinic voices and communities who hold that engaged study of sacrificial law itself constitutes fulfillment are structurally excluded from setting the dominant doctrinal frame in performance-only institutions; their view is treated as consolation or minority opinion rather than as a live alternative with equal claim on the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, study_as_performance_proponents, excluded,
    moderate, generational, constrained, global).

% Scholars of halakhic development trace how the performance-only doctrine hardened after the Temple's destruction and examine its institutional consequences, without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, messianic_restorationist_authorities).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a sharp, unambiguous criterion — physical execution at a standing Temple altar — for what counts as fulfilling the sacrificial commandments, preventing doctrinal drift where any activity could be claimed as satisfying the obligation.
% TRANSFER_FUNCTION: Moves religious legitimacy and the sense of 'genuine' fulfillment away from diaspora study communities and lay practitioners (who cannot perform the acts) toward restorationist authorities and reconstruction institutions (whose mission depends on the acts remaining unperformed and awaited).
% ABSENT_VOICES: Proponents of the study_as_performance reading are marginalized within performance-only institutions; they would argue that treating 1,900 years of rigorous study as non-fulfillment devalues the actual religious labor diaspora communities perform, but their view rarely governs curriculum or doctrine in restorationist-aligned settings.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished and study_as_performance became dominant, restorationist institutions would lose a distinctive claim to being the sole path to fulfillment, and diaspora study communities' labor would be revalued as itself completing the commandment rather than merely preparing for it. Whether the 'world rearranges' depends on which party is asked: restorationist authorities would say their entire mission is voided; study communities would say nothing changes in practice, only in doctrinal status.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the rabbinic tradition needed to determine whether commandments requiring the Temple were annulled, transformed, or merely suspended — preserving both the commandments' ongoing validity and a coherent account of why they could not currently be performed.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist authorities attest the founding problem remains fully live (the Temple has not been rebuilt, so suspension continues to apply exactly as originally reasoned). Independent historians of halakha, outside both the restorationist and study-as-performance camps, note that the doctrine's insistence on strict physical-performance criteria hardened over centuries in ways that served institutional continuity and messianic anticipation as much as it reflected an unavoidable textual reading — no source entirely outside a benefiting party corroborates the doctrine as the only possible resolution.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 at destruction to 0.71 by the modern era) because the doctrinal insistence on physical-performance-only became more institutionally entrenched as messianic and restorationist movements organized around it, converting a circumstantial ruling (ones) into a load-bearing pillar of religious identity that channels scholarly and emotional investment toward an act perpetually deferred. Theater ratio also rises (0.25 to 0.62) as the study of sacrificial law increasingly serves symbolic/devotional functions — demonstrating piety and preserving communal identity — that are decoupled from any live prospect of the acts being performed; the ritual of studying Zevachim in Tisha B'Av-adjacent settings, for instance, is substantially performative of loss and hope rather than functionally preparatory. Suppression (0.58) reflects moderate but real doctrinal pressure against reframing study itself as fulfillment — a pressure that stabilizes rather than escalates, since it need not intensify to remain effective once the doctrine is institutionally settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Restorationist authorities and reconstruction institutions sit near the beneficiary end: their legitimacy and mission-relevance are strengthened, not weakened, by the commandment remaining unperformed and by study being denied equivalent status. Diaspora study communities and lay practitioners sit near the target end: their labor and their religious aspiration are structurally foreclosed from counting as fulfillment, with no individual action available to close the gap. This is directionality derived from role, not from any claim about the sincerity of any party's belief.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to preserve the validity and coherence of commandments that became literally impossible to perform — was live and urgent in 70-200 CE. Whether it remains equally live 1,900 years later, when the doctrine has become a stable institutional anchor for restorationist identity rather than an active crisis-management tool, is exactly the contested status recorded in founding_problem_status. The performance_only reading does not resolve this ambiguity; it is one live answer, sustained by parties who benefit from treating the suspension as ongoing and unresolved rather than as a historical artifact absorbed into settled practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_versus_annulment_ambiguity,
    'Is the sacrifice commandment genuinely suspended (ones — a temporary circumstantial bar, in principle reversible) or has 1,900 years of non-performance functionally annulled it in practice, with the suspension framing serving mainly to preserve doctrinal continuity and restorationist legitimacy?',
    'Comparative analysis of how other ''suspended'' commandments in Jewish law have been treated over comparably long periods, and examination of whether authoritative sources ever revisit the suspension/annulment boundary based on duration rather than treating it as fixed at the moment of destruction.',
    'If functionally annulled, the performance_only doctrine''s ongoing operation is better described as institutional maintenance of a category than as live halakhic reasoning, strengthening the case that its persistence serves restorationist institutional interests. If genuinely suspended and reversible in principle, the doctrine remains a coherent, non-extractive holding position regardless of duration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_versus_annulment_ambiguity, conceptual, 'Whether multi-millennial suspension has become functionally indistinguishable from annulment.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the performance_only, study_as_performance, and archive_maintenance readings diverge — is it a disagreement about what ''fulfillment'' textually requires, about whether intention/engagement can substitute for physical acts generally in halakha, or about the practical stakes of getting the classification wrong (e.g., liability for a failed mitzvah)?',
    'Close comparative reading of the classical sources each reading cites (e.g., treatment of tefillin study vs. performance, prayer as a substitute for sacrifice in existing tradition) to isolate the specific textual or logical point of divergence.',
    'If the disagreement is narrowly textual, the three readings may be reconcilable in ways that reduce this reading''s claimed extraction; if it reflects a deep disagreement about the nature of mitzvah-fulfillment itself, the three constraints are genuinely structurally distinct and this reading''s high-ε status is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between sibling kernel readings.').

omega_variable(
    institutional_benefit_versus_doctrinal_necessity,
    'Does the performance_only reading persist because it is the doctrinally compelled reading of the sources, or because it structurally benefits restorationist and reconstruction institutions regardless of its textual necessity?',
    'Examine whether communities and authorities with no institutional stake in Temple restoration (e.g., non-restorationist diaspora communities) independently arrive at and maintain the performance_only reading, or whether it correlates specifically with restorationist affiliation.',
    'Independent convergence would support the reading as doctrinally driven rather than interest-driven, lowering confidence in the tangled_rope classification; correlation with institutional benefit would support the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_versus_doctrinal_necessity, empirical, 'Whether doctrinal necessity or institutional interest better explains the reading''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_commandment__performance_only, theater_ratio, 300, 0.35).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_commandment__performance_only, theater_ratio, 700, 0.45).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_commandment__performance_only, theater_ratio, 1100, 0.52).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.58).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.62).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t300, sacrifice_commandment__performance_only, base_extractiveness, 300, 0.48).
narrative_ontology:measurement(sacr_be_t700, sacrifice_commandment__performance_only, base_extractiveness, 700, 0.58).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_commandment__performance_only, base_extractiveness, 1100, 0.63).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sacr_su_t300, sacrifice_commandment__performance_only, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(sacr_su_t700, sacrifice_commandment__performance_only, suppression_requirement, 700, 0.5).
narrative_ontology:measurement(sacr_su_t1100, sacrifice_commandment__performance_only, suppression_requirement, 1100, 0.53).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.56).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the single natural-language label 'the obligation to bring sacrifices absent the Temple' (the sacrifice_commandment kernel). performance_only authors the highest ε because it uniquely combines denial of study's fulfillment-status with treatment of the resulting incompletion as an unresolved ongoing cost. study_as_performance authors low ε because study itself counts as satisfying the obligation, eliminating the extraction this reading identifies. archive_maintenance authors a different, lower-stakes profile centered on preservation-function rather than either fulfillment or extraction. All three share the fixed kernel text and Temple-destruction reference frame but diverge on the fulfillment criterion, and are linked here per the ε-invariance decomposition rule rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
