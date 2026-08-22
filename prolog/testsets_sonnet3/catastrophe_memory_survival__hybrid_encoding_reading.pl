% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Ritual as Hybrid Encoding: Symbolic Boundary-Maintenance Plus Embedded Practical Knowledge
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the hybrid_encoding_reading of the
 *   catastrophe_memory_survival kernel: rituals that emerge from communal
 *   catastrophe (flood cycles, famine, epidemic, displacement) are analyzed
 *   as operating simultaneously on two registers — symbolic
 *   boundary-maintenance (identity, taboo, belonging) and embedded practical
 *   knowledge (timing, resource protocols, adaptive strategy) — with the
 *   claim that survival depends on BOTH registers functioning together, not
 *   on either alone. This reading deliberately refuses to collapse the ritual
 *   into either a purely symbolic-anthropology account (the
 *   symbol_survival_reading) or a purely functionalist
 *   traditional-ecological-knowledge account (the
 *   competence_transmission_reading). ε is authored low because, under this
 *   reading's own lights, the hybrid arrangement is not extractive: the
 *   community that maintains the ambiguity is the beneficiary, and the cost
 *   of the arrangement falls almost entirely on external institutional
 *   classifiers who must force a binary the community itself does not need.
 *
 * KEY AGENTS:
 *   - practicing_communities: primary beneficiary — hold both registers without needing theoretical resolution
 *   - elder_practitioners: agenda-setters who transmit the fused content directly
 *   - binary_classification_analysts: payers — bear the cost of forced single-register classification
 *   - development_and_heritage_institutions: excluded downstream funders whose categories cannot hold the hybrid
 *   - comparative_ritual_scholars: analytical observers documenting the hybrid structure across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Ritual as Hybrid Encoding: Symbolic Boundary-Maintenance Plus Embedded Practical Knowledge").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '8c77ff71-cc07-47c4-8f9e-966ccd15cb29').
narrative_ontology:cs_kernel_codification('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', distributed).
narrative_ontology:cs_authority_grounding('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', practice).
narrative_ontology:cs_interpretation_layer_present('8c77ff71-cc07-47c4-8f9e-966ccd15cb29').
narrative_ontology:cs_reading_relation('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', foundational, joint_necessity_of_dual_registers).
narrative_ontology:cs_axiom_status(joint_necessity_of_dual_registers, holdable).
narrative_ontology:cs_axiom_grounding('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', joint_necessity_of_dual_registers, empirically_contingent).
narrative_ontology:cs_axiom('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', foundational, classification_forcing_is_the_locus_of_cost_not_community_practice).
narrative_ontology:cs_axiom_status(classification_forcing_is_the_locus_of_cost_not_community_practice, holdable).
narrative_ontology:cs_axiom_grounding('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', classification_forcing_is_the_locus_of_cost_not_community_practice, conventional).
narrative_ontology:cs_reference_frame('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', unresolved_dual_register_practice).
narrative_ontology:cs_drift_state('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', contemporary_heritage_and_development_intervention_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8c77ff71-cc07-47c4-8f9e-966ccd15cb29', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, elder_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_encoding_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform, transmit, and revise the ritual across generations without needing to resolve, in theory, whether it is 'really' about symbolic boundary-maintenance or practical survival knowledge. The community holds and uses both registers simultaneously — the timing rules embedded in a harvest ceremony carry both agronomic information and identity-marking function at once. They benefit from NOT having to choose: the hybrid form is more robust and more transmissible than either register alone would be, and forcing a choice would degrade the ritual's actual function in their lives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities, agenda_setter).

% Hold and transmit the ritual's full content — sequence, timing, taboo, and story together — to the next generation. They do not separate the practical instruction from the symbolic frame when teaching; the two are taught as one act. Their authority and their community's survival competence both ride on keeping the registers fused.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, elder_practitioners, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, elder_practitioners, beneficiary).

% Academics, policy bodies, and heritage institutions who must classify the ritual as either 'symbolic/cultural' (for heritage preservation funding, museification, or religious-studies analysis) or 'practical/adaptive' (for disaster-risk-reduction funding, indigenous-knowledge documentation, or agricultural-extension programs) because their institutional categories and funding streams require a single answer. Forcing the binary costs them explanatory accuracy: whichever register they select as primary, the other's real causal contribution to survival becomes invisible in their models, and their resulting interventions (extracting the 'practical knowledge' into a manual, or preserving the 'symbolic form' as heritage theater) tend to break the very hybrid structure that made the ritual work.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts, payer,
    institutional, biographical, mobile, global).

% Downstream funders and program designers who inherit whichever binary classification the analysts settle on. They are not in the room when the community actually performs the ritual, and their program categories (heritage grant vs. resilience grant) structurally cannot accommodate an unresolved hybrid, so their voice in favor of maintaining ambiguity is never solicited.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, development_and_heritage_institutions, excluded,
    institutional, biographical, mobile, global).

% Study the ritual across cases and argue that dual-register hybridity is itself the structurally correct description, resisting pressure from their own disciplines to collapse the case into either the symbolic-anthropology camp or the traditional-ecological-knowledge camp.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, comparative_ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual coordinates the community's transmission of both boundary-identity (who we are, what separates us from outsiders/the past catastrophe) and practical survival competence (when to plant, move, store, warn) in a single, low-cost, memorable, repeatable act — solving two transmission problems with one mechanism rather than requiring two separate institutions.
% TRANSFER_FUNCTION: No net extractive transfer between registers or between community members; what the arrangement moves is interpretive burden — away from the community (who use the hybrid without needing to name it) and onto external classifiers (who must force it into a single institutional category to fund or study it).
% ABSENT_VOICES: Development and heritage institutions that fund single-register interventions are structurally absent from the community's own transmission process, so their preference for a clean binary is never tested against the community's lived practice; the community itself is rarely asked whether it experiences the ritual as one thing or two.
% DISAPPEARANCE_RATIONALE: If the hybrid-encoding structure were dissolved (i.e., if the practical and symbolic registers were successfully separated and transmitted independently), transmission efficiency would drop: the mnemonic and motivational power of the symbolic frame would no longer carry the practical content, and the practical content, extracted into a manual or advisory, would lose the community buy-in and correct-timing enforcement that ritual performance provides. Communities would need to build two institutions where one sufficed.
% FOUNDING_PROBLEM: A community facing catastrophe (flood, famine, drought cycle, epidemic) needed a transmission mechanism robust enough to survive disruption of formal instruction (schools, written records, specialist elders lost) that could still carry forward both practical adaptive knowledge and the identity/cohesion needed to act on it collectively.
% FOUNDING_PROBLEM_CORROBORATION: Comparative ritual scholars and ethnobiologists working from outside the practicing communities (documenting agronomic accuracy embedded in ceremonial timing across independent case studies) corroborate that the practical-knowledge function remains operative, not merely nostalgic; oral historians within the communities corroborate the identity/boundary function remains operative. No corroboration exists from the binary_classification_analysts' own institutional record, since their categories cannot register the hybrid claim in the first place — that absence is itself part of what this reading documents.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, from this reading's referent — the standing hybrid arrangement as it actually operates in the community — no party is extracting rent from another via the ritual's dual-register structure; the 'cost' identified by this reading is an epistemic cost borne by external classifiers, not a resource transfer within the community. Suppression is low (0.15): nothing coercive holds the hybrid together beyond ordinary transmission practice. Theater ratio is modest and slowly rising (0.15 to 0.20) reflecting a plausible mild drift toward more performative articulation of the ritual as external audiences (tourists, researchers, funders) increasingly observe it, without yet approaching a piton profile. Resistance (0.35) and accessibility_collapse (0.3) are both moderate-low: alternative framings (pure symbol, pure competence) are readily available in the literature and contested, so collapse is far from complete, consistent with a rope rather than a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Practicing communities and elder-practitioners sit near the beneficiary end: the hybrid form serves their actual transmission needs and they pay no theoretical tax for using it. Binary_classification_analysts sit as the structural targets of this reading's claim — their institutional need for a clean category is what the hybrid structure resists, and the cost of resisting it (grant proposals that don't fit templates, papers that get rejected from single-discipline journals, interventions that misfire when they extract only one register) falls on them. Development_and_heritage_institutions are excluded rather than paying directly — their programs simply never encounter the correct hybrid description because their categories foreclose it upstream.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_encoding_reading exists specifically to prevent mislabeling: it argues that treating the ritual as pure coordination-for-identity (rope in the symbolic register alone) OR as pure extraction-disguised-as-tradition (a snare reading that would follow if practical content were shown to be faked or self-serving) both mischaracterize a structure that is genuinely doing two jobs at once. Classifying THIS reading as a low-ε rope is not a claim that the ritual has no costs anywhere — the sibling readings may authored different ε for their own referents — but a claim that, described on its own hybrid terms, the arrangement is closer to genuine low-coercion coordination than either single-register account would suggest in isolation, because no single beneficiary is capturing a rent that the other register's absence would otherwise reveal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_committer_structure,
    'Is the dual-register hybrid claim a genuinely distinct structural description of the ritual, or is it a synthesis position that borrows legitimacy from both sibling readings without adding independent explanatory content?',
    'Cross-case comparative analysis: if cases exist where the symbolic register persists after the practical register has become causally inert (e.g., the timing no longer tracks any real agronomic signal) and the ritual still functions for survival via cohesion alone, or vice versa, that would support treating the registers as separable rather than jointly necessary, undermining the hybrid reading''s core premise. If no such decoupled cases exist, the hybrid reading is corroborated as structurally necessary rather than a rhetorical synthesis.',
    'If decoupling is empirically observed, this reading''s premise (both registers are jointly necessary for survival) weakens, and one of the sibling readings becomes the more accurate structural account for that case, changing which reading properly applies rather than changing this reading''s own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_committer_structure, conceptual, 'Whether hybrid encoding is a real structural feature or an interpretive synthesis of two more basic readings').

omega_variable(
    sibling_reading_disagreement_location,
    'Where exactly do the three kernel readings disagree — on what the ritual DOES (empirical), or on which effect counts as the ritual''s REAL function (normative/definitional)?',
    'Separate the empirical question (does the ritual carry information with measurable predictive accuracy about resource timing, AND does it maintain measurable group cohesion/boundary markers) from the definitional question (which of these, if both are present, counts as ''what survival depends on''). The first is resolvable by field data; the second is not.',
    'If the disagreement is purely definitional, all three readings can be simultaneously empirically correct while remaining distinct constraints (as this framework requires) — the kernel is genuinely under-determined at the level of ultimate function-attribution even when the underlying facts are settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_location, conceptual, 'Locating whether the kernel readings differ empirically or only in functional attribution').

omega_variable(
    analyst_victim_status_ambiguity,
    'Is classifying binary_classification_analysts as victims of this arrangement appropriate, given they are institutionally powerful actors making voluntary category choices, or does this framing overstate their cost relative to the community''s structural position?',
    'Assess whether the analysts'' classification failure produces material downstream harm to the communities themselves (misdirected funding, program failure) versus purely an academic/institutional inconvenience contained within the analysts'' own sphere.',
    'If the analysts'' misclassification materially harms the community (e.g., a resilience program built on the wrong register fails and communities bear the consequence), the true victim set should include the communities as secondary victims, which would raise this reading''s ε and complicate its rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(analyst_victim_status_ambiguity, empirical, 'Whether institutional analysts are the true cost-bearers or merely a proxy for community-level harm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language concept 'ritual as catastrophe memory' under the ε-invariance principle. symbol_survival_reading assigns primacy to boundary/identity maintenance; competence_transmission_reading assigns primacy to embedded practical knowledge; hybrid_encoding_reading (this story) claims both are jointly necessary and locates the reading's own extraction/victim structure not within the community but among external analysts forced into single-register classification. Each sibling authors its own ε, beneficiary/victim set, and claimed_type against its own referent (the standing arrangement under contest, per its own reading); they are linked here rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
