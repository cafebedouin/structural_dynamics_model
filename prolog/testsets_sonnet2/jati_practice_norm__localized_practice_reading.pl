% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundary as Locally Renegotiated Coordination Norm
 *   domain: social/religious/economic
 *
 * SUMMARY:
 *   This story authors the localized-practice reading of the jati-boundary
 *   kernel: jati categories are treated as living, locally-adjudicated
 *   coordination norms that proliferate, split, merge, and renegotiate
 *   continuously in response to migration, economic mobility, and
 *   inter-household bargaining. The empirical signature this reading is built
 *   to explain is the observed proliferation of jati categories into the
 *   thousands across census and ethnographic records — a pattern that a fixed
 *   scriptural-varna account or a purely externally-imposed administrative
 *   account struggles to explain, but that a continuous local-renegotiation
 *   account predicts directly. Extraction under this reading is low: the
 *   primary function is coordination of marriage pools, occupational mutual
 *   aid, and reputational information, with genuine benefit flowing to most
 *   participants and no centralized enforcement apparatus compelling
 *   compliance. Real costs exist at the margin (boundary challengers,
 *   inter-jati couples) but they are local, socially-mediated, and not backed
 *   by an active administrative enforcement machinery — consistent with a
 *   rope rather than a tangled_rope or snare reading.
 *
 * KEY AGENTS:
 *   - local_jati_councils: primary agenda-setters, locally bounded authority (organized/constrained)
 *   - marriage_networks and occupational_guild_members: primary beneficiaries of the coordination function
 *   - newly_forming_subcastes: agents actively driving proliferation, evidence for weak/negotiable enforcement
 *   - boundary_challengers and inter_jati_couples: bear localized social costs at the margin
 *   - social_anthropologists: analytical observers whose fieldwork grounds this reading empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.22).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.28).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary as Locally Renegotiated Coordination Norm").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/economic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '4610a611-f690-44e6-b2fb-89215181f96a').
narrative_ontology:cs_kernel_codification('4610a611-f690-44e6-b2fb-89215181f96a', distributed).
narrative_ontology:cs_authority_grounding('4610a611-f690-44e6-b2fb-89215181f96a', practice).
narrative_ontology:cs_interpretation_layer_present('4610a611-f690-44e6-b2fb-89215181f96a').
narrative_ontology:cs_reading_relation('4610a611-f690-44e6-b2fb-89215181f96a', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('4610a611-f690-44e6-b2fb-89215181f96a', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('4610a611-f690-44e6-b2fb-89215181f96a', foundational, boundary_legitimacy_derives_from_ongoing_local_consensus).
narrative_ontology:cs_axiom_status(boundary_legitimacy_derives_from_ongoing_local_consensus, holdable).
narrative_ontology:cs_axiom_grounding('4610a611-f690-44e6-b2fb-89215181f96a', boundary_legitimacy_derives_from_ongoing_local_consensus, conventional).
narrative_ontology:cs_axiom('4610a611-f690-44e6-b2fb-89215181f96a', secondary, proliferation_evidences_coordination_not_pollution).
narrative_ontology:cs_axiom_status(proliferation_evidences_coordination_not_pollution, holdable).
narrative_ontology:cs_axiom_grounding('4610a611-f690-44e6-b2fb-89215181f96a', proliferation_evidences_coordination_not_pollution, empirically_contingent).
narrative_ontology:cs_reference_frame('4610a611-f690-44e6-b2fb-89215181f96a', pre_colonial_locally_adjudicated_practice).
narrative_ontology:cs_drift_state('4610a611-f690-44e6-b2fb-89215181f96a', contemporary_diaspora_and_urban_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4610a611-f690-44e6-b2fb-89215181f96a', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_councils).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, marriage_networks).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_guild_members).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, newly_forming_subcastes).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, boundary_challengers).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, inter_jati_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, occupational_guild_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate disputes over commensality, marriage eligibility, and occupational claims within a village or regional cluster. Regularly revise who counts as within the jati boundary in response to migration, economic mobility, and disputes. Their authority is real but locally bounded and contestable by rival councils or breakaway factions.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_councils, agenda_setter,
    organized, generational, constrained, local).

% Use jati boundaries to coordinate a legible pool of marriage partners, sharing information about lineage, reputation, and ritual status across households that would otherwise have no efficient way to vet each other. Benefit from the boundary's stability but also from its capacity to expand or subdivide to admit new families through negotiation, migration, or improved economic standing.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, marriage_networks, beneficiary,
    moderate, biographical, constrained, regional).

% Historically organized around a craft or trade jati identity that coordinated skill transmission, mutual aid, and market reputation. Some now find the occupational tie obsolete as members diversify into other work, and negotiate new sub-jati labels or reinterpretations of the boundary to keep the coordination benefits (mutual aid networks, marriage pool) while shedding the occupational constraint.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_guild_members, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, occupational_guild_members, payer).

% Groups that have split from, merged with, or newly claimed a jati designation — often correlating with a shift in economic fortune, migration, or a claim to higher ritual status. The empirically observed proliferation to thousands of distinct jati categories across regions is substantially driven by this kind of active, bottom-up boundary renegotiation rather than top-down imposition.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, newly_forming_subcastes, beneficiary,
    moderate, generational, mobile, local).

% Individuals or small families who attempt to claim a status upgrade, cross into a different jati's marriage pool, or reject the boundary altogether. They bear the local social cost of contesting a norm that most of their community still treats as operative, even though the norm itself has no textual fixity and is known to have shifted before for other groups.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, boundary_challengers, payer,
    powerless, biographical, constrained, local).

% Couples who marry across jati lines face local social sanction — exclusion from certain community functions, family estrangement, occasional violence in extreme cases — even though the boundary they crossed is, on this reading, a negotiated local norm rather than a scriptural mandate. Their situation shows that 'coordination norm' framing does not mean costless for everyone at every moment; low aggregate extraction does not mean zero extraction at the margin.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, inter_jati_couples, payer,
    powerless, biographical, trapped, local).

% Document ethnographic case studies showing jati boundaries shifting within living memory: castes splitting, merging, upgrading claimed varna affiliation (Sanskritization), and multiplying into locally specific categories uncountable from any single fixed list. Their fieldwork is the primary evidentiary basis for this reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, social_anthropologists, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, locally-administered mechanism for coordinating marriage pools, mutual aid networks, occupational skill transmission, and ritual participation among households that would otherwise lack efficient means of establishing mutual trust and reciprocal obligation.
% TRANSFER_FUNCTION: Primarily coordinates information and reciprocal obligation rather than transferring resources upward; where transfer occurs it moves social standing and marriage-pool access toward negotiating groups that successfully claim boundary revision, and imposes reputational and relational costs on those who violate the locally operative boundary at a given moment.
% ABSENT_VOICES: Individuals permanently excluded from any jati network (including some historically marginalized groups whose exclusion is not a matter of ongoing negotiation but of structural closure) are largely outside this reading's frame, which emphasizes fluidity; their experience of the boundary as fixed and closed rather than negotiable is not well captured here and belongs more to the orthodox_textual_reading's victim set.
% DISAPPEARANCE_RATIONALE: If jati boundaries as locally-adjudicated coordination norms vanished overnight, marriage networks would lose their primary mechanism for reputational vetting, occupational mutual-aid structures would need replacement, and local councils would lose their adjudicating function — communities would need to reconstruct trust-coordination mechanisms from scratch or via alternative (e.g. purely economic or state-administered) means.
% FOUNDING_PROBLEM: Pre-modern and early modern communities needed a decentralized way to coordinate marriage eligibility, occupational specialization and skill transmission, and mutual aid obligations in the absence of a strong centralized state or market institutions capable of performing these functions.
% FOUNDING_PROBLEM_CORROBORATION: Social anthropologists conducting village-level fieldwork attest that local councils continue to actively perform real coordination functions (marriage vetting, dispute adjudication, mutual aid) in the present day — this is corroboration from outside the beneficiary groups themselves. However, boundary_challengers and inter_jati_couples, also outside the beneficiary set, attest that the same mechanism currently functions primarily as an exclusionary cost imposed on non-conforming individuals, suggesting the founding coordination problem has been substantially solved by modern state, market, and communication institutions and the residual function is now partly custodial persistence rather than live necessity.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22-0.28) and declining slightly over the interval, reflecting a reading in which the norm's coordination function dominates and enforcement capacity to extract has never been strong nor institutionally centralized. Suppression (0.28) and accessibility_collapse (0.35) are moderate-low: local social sanction is real for boundary_challengers and inter_jati_couples but does not amount to a totalizing structural barrier — the very existence of thousands of locally-distinct jati categories is direct evidence that boundaries move, split, and are renegotiated rather than being rigidly enforced from a single fixed point. Theater_ratio is low and falling (0.20 to 0.15): the councils' adjudicating activity is substantially functional (real disputes, real marriage-pool coordination) rather than performative maintenance of an empty form. Resistance (0.4) reflects genuine but localized friction — boundary_challengers and inter_jati couples do push back, and some of that resistance succeeds over time (hence extraction declining slightly), which is itself part of what this reading predicts.
 *
 * DIRECTIONALITY LOGIC:
 *   local_jati_councils sit near the agenda-setting end but are themselves constrained by rival councils and shifting community consensus, not a fixed hierarchy — this differentiates them structurally from a colonial-census-reading administrator, who would have externally-backed coercive capacity this reading's councils lack. marriage_networks, occupational_guild_members, and newly_forming_subcastes are beneficiaries whose exit options are constrained but who actively use renegotiation to improve their position, which is inconsistent with a high-suppression/high-extraction snare reading. boundary_challengers and inter_jati_couples are the clearest payers: powerless, locally trapped or constrained, bearing costs that are real but socially rather than administratively enforced.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (marriage vetting, occupational mutual aid, dispute resolution absent strong centralized institutions) is genuinely contested as to whether it remains live: modern state institutions, labor markets, and communication technology have substantially displaced the practical necessity of jati-based coordination in many urban contexts, while it remains functionally load-bearing in many rural and diaspora contexts. This reading treats the arrangement as still substantially coordination-dominant rather than declaring it mandatrophic, because the empirical proliferation pattern (active, ongoing local renegotiation) is inconsistent with a purely inertial, performance-only residue — an inert vestige would not still be generating thousands of new locally-negotiated distinctions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proliferation_as_weak_enforcement_vs_local_capture,
    'Does the empirical proliferation of jati categories into the thousands indicate genuinely weak, coordination-dominant enforcement (this reading''s claim), or does it instead reflect thousands of independently-operating LOCAL extraction structures, each individually tangled_rope-like even though no single central authority enforces a unified boundary?',
    'Micro-level ethnographic comparison of enforcement intensity and cost-to-challengers across a representative sample of local jati councils; if most local instances show high suppression and identifiable victim cost concentrated on boundary challengers, the aggregate low-extraction reading may be an artifact of averaging over local extraction rather than an accurate description of any single instance.',
    'If local capture is the dominant pattern, this reading''s aggregate rope classification would mask a fractal snare/tangled_rope structure at the local level, which would not be visible from macro-level proliferation data alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_as_weak_enforcement_vs_local_capture, empirical, 'Whether observed proliferation reflects genuine weak enforcement or masks many independent local extraction structures.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and the sibling readings (orthodox_textual_reading, colonial_census_reading) actually live — is it a disagreement about the SAME set of empirical facts interpreted differently, or about which historical period/region each reading is actually describing?',
    'Cross-reading historiographic analysis: compare the specific case studies and time periods each reading''s evidentiary base draws from (textual reading draws on dharmashastra literature; colonial reading draws on 19th/20th century census records; this reading draws on contemporary and pre-colonial ethnography) to determine whether the readings are genuinely rival interpretations of one phenomenon or descriptions of different phases of a single longer historical process.',
    'If the readings describe different historical phases rather than rival interpretations of the same phase, the kernel itself may need to be split temporally rather than treated as one contested kernel with three simultaneous readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the three sibling readings disagree about the same evidence or describe different historical periods.').

omega_variable(
    inter_jati_couple_cost_severity,
    'Is the social cost borne by inter_jati_couples and boundary_challengers small and diminishing (consistent with a genuine rope), or does it include a tail of severe cases (family violence, honor-based sanction) that would push the effective extraction and suppression for this specific subgroup into snare territory even while the aggregate reading remains rope-like?',
    'Disaggregated incident data on social sanction severity against inter-jati couples across regions and time, distinguishing modal outcomes (social friction, temporary estrangement) from tail outcomes (violence, permanent ostracism).',
    'A significant severe-tail would argue for authoring inter_jati_couples as a distinct, more extractive sub-constraint rather than folding their situation into this rope-classified aggregate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_jati_couple_cost_severity, empirical, 'Whether costs to inter-jati couples are uniformly mild or include a severe tail requiring separate treatment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__localized_practice_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__localized_practice_reading, theater_ratio, 50, 0.155).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__localized_practice_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__localized_practice_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jati_practice_norm__localized_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.1).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the jati_practice_norm kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: orthodox_textual_reading (fixed scriptural derivation, high accessibility_collapse, low proliferation expected), colonial_census_reading (externally imposed administrative reification, moderate-high extraction via governance-legibility function), and this localized_practice_reading (low extraction, coordination-dominant, high proliferation expected as evidence of weak central enforcement). The three do not share an ε value; each is assessed independently and linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
