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
 *   human_readable: Jati Boundary Norms as Localized, Renegotiable Coordination (Practice Reading)
 *   domain: social/religious/economic
 *
 * SUMMARY:
 *   This story authors the localized_practice_reading of the
 *   jati_practice_norm kernel: jati boundaries as continuously renegotiated,
 *   locally variable coordination conventions rather than a fixed scriptural
 *   hierarchy (orthodox_textual_reading) or a category system stabilized by
 *   colonial administrative classification (colonial_census_reading). The
 *   empirical signature this reading emphasizes is proliferation — recorded
 *   jati names number in the thousands, and local councils are documented
 *   splitting, merging, and reclassifying groups across generations, which is
 *   inconsistent with either a single fixed textual framework or a durably
 *   stabilized census taxonomy. Under this reading the extraction is low and
 *   locally contingent: real costs fall on boundary-crossers and locally
 *   low-ranked subgroups, but the mechanism is diffuse social enforcement
 *   rather than centralized coercion, and the boundary itself is not stable
 *   enough across localities to sustain systematic, large-scale extraction.
 *   This story's ε is authored strictly for the localized-practice
 *   arrangement as this reading sees it — it does not average across, or
 *   borrow ε from, the textual or census siblings, which are separate
 *   constraints with their own files.
 *
 * KEY AGENTS:
 *   - local_jati_councils: agenda_setter (organized/constrained) — adjudicate and renegotiate boundaries locality by locality
 *   - endogamous_kin_networks: beneficiary (moderate/constrained) — use the norm for marriage and trust coordination
 *   - occupational_guild_clusters: beneficiary/agenda_setter (organized/constrained) — use and actively renegotiate boundaries for occupational protection and status mobility
 *   - boundary_crossing_couples: payer (powerless/trapped) — bear locally enforced exclusion despite the norm's overall low stability
 *   - lowest_status_subgroups_within_local_hierarchy: payer (powerless/constrained) — bear locality-specific exclusion
 *   - ethnographers_and_census_analysts: observer (analytical) — document proliferation and instability as evidence against fixed-boundary readings
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
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary Norms as Localized, Renegotiable Coordination (Practice Reading)").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/economic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6').
narrative_ontology:cs_kernel_codification('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', distributed).
narrative_ontology:cs_authority_grounding('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', practice).
narrative_ontology:cs_interpretation_layer_present('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6').
narrative_ontology:cs_reading_relation('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', foundational, boundary_legitimacy_derives_from_ongoing_local_consensus).
narrative_ontology:cs_axiom_status(boundary_legitimacy_derives_from_ongoing_local_consensus, holdable).
narrative_ontology:cs_axiom_grounding('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', boundary_legitimacy_derives_from_ongoing_local_consensus, conventional).
narrative_ontology:cs_axiom('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', secondary, categorical_proliferation_evidences_absence_of_fixed_kernel).
narrative_ontology:cs_axiom_status(categorical_proliferation_evidences_absence_of_fixed_kernel, holdable).
narrative_ontology:cs_axiom_grounding('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', categorical_proliferation_evidences_absence_of_fixed_kernel, empirically_contingent).
narrative_ontology:cs_created_at('67672ae6-2044-45bf-9a7c-6fbe3c5ecbb6', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_councils).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, endogamous_kin_networks).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_guild_clusters).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, boundary_crossing_couples).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, lowest_status_subgroups_within_local_hierarchy).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, jati_as_fluid_ethnosociological_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village or sub-regional bodies that adjudicate marriage eligibility, commensality, and occupational boundaries case by case. They renegotiate who counts as within the jati as economic circumstances, migration, and intermarriage pressure the boundary, and they have historically split, merged, or renamed jatis (accounting for the proliferation into thousands of recorded categories). Their authority is local and contestable, not backed by a central text or state apparatus.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_councils, agenda_setter,
    organized, generational, constrained, regional).

% Extended kin groups that use jati boundaries to coordinate marriage alliances, mutual aid, and reputational trust within a bounded pool. They benefit from a legible, if locally variable, set of rules for who is a suitable marriage partner or credit-worthy trading partner; the coordination reduces search and verification costs for cooperation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, endogamous_kin_networks, beneficiary,
    moderate, biographical, constrained, local).

% Trade- and craft-linked jati segments that use boundary norms to regulate entry into a occupational niche, protect skill transmission, and coordinate pricing or apprenticeship norms among members. They actively participate in renaming and repositioning their group's status claims (Sanskritization-type mobility strategies), which is itself evidence the boundary is negotiated rather than fixed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_guild_clusters, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, occupational_guild_clusters, agenda_setter).

% Individuals who marry or partner across jati lines within a locality where the local council still enforces endogamy socially (ostracism, loss of ritual and economic ties) even though no central authority commands it. Their exit is bounded by locality: a boundary loosely enforced in one village may be tightly held in the next, and migrating to escape one council's judgment often just relocates them under another's.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, boundary_crossing_couples, payer,
    powerless, biographical, trapped, local).

% Subgroups positioned at the bottom of a given locality's jati ranking bear exclusion from wells, temples, or commensal exchange even where the boundary itself is locally negotiated and variable elsewhere. Their remedy is typically emigration to a locality where their subgroup's local ranking is less severe, or collective status-mobility campaigns over generations — both slow, uncertain routes.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, lowest_status_subgroups_within_local_hierarchy, payer,
    powerless, generational, constrained, local).

% Document the empirical proliferation of jati names (into the thousands) and the constant splitting, merging, and status-claiming that occurs locality by locality, using this as evidence against a fixed textual or administratively stabilized boundary.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, ethnographers_and_census_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible (if locally variable) basis for coordinating marriage alliance, mutual aid networks, credit and trust relationships, and occupational entry/exit within a bounded reference group, without requiring a central registry or authority to adjudicate every case.
% TRANSFER_FUNCTION: Where enforced locally, moves social and economic goods (marriage eligibility, commensal access, occupational entry, reputational standing) preferentially toward members of the locally dominant jati grouping and away from those positioned as boundary-crossers or lower-ranked subgroups in that specific locality.
% ABSENT_VOICES: Individuals whose status claims are rejected by a given local council (failed Sanskritization attempts, contested subgroup mergers) have no venue above the locality to appeal to; they are heard only if a rival local faction takes up their claim for its own reasons.
% DISAPPEARANCE_RATIONALE: Practice-reading advocates would argue that if the norm-enforcement apparatus vanished overnight, local marriage and trust networks would need to rapidly substitute other legibility mechanisms (reputation, formal contract, state registries), producing real short-term disruption in rural areas where such substitutes are thin — but they would also argue the boundaries themselves would simply continue mutating and re-forming under new names, since the underlying coordination problem (who is a reliable ally) does not disappear even if THIS particular boundary-drawing convention did.
% FOUNDING_PROBLEM: In stateless or weakly-institutionalized agrarian localities, kin and occupational groups needed a low-cost, locally legible way to coordinate marriage alliance, credit, and craft transmission without a central registry — jati boundaries functioned as that legibility device, continuously renegotiated as local conditions (migration, trade, status competition) shifted.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographers and census analysts (outside the jati councils that benefit from the boundary) attest that the coordination problem is partly live — informal economies in many localities still lack alternative trust infrastructure — but also document that state institutions, formal credit markets, and civil marriage law increasingly substitute for the function in urban and mobile populations, making the founding problem's continued relevance geographically and economically uneven rather than uniformly live or dead.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, contested).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.22) and roughly flat over the century-scale interval because the practice-reading's central empirical claim is that the boundary itself is too locally variable and continuously renegotiated to sustain a stable extraction gradient at scale — proliferation to 3000+ named categories evidences weak, fragmented enforcement rather than a coordinated extraction apparatus. Suppression (0.28) and resistance (0.4) are moderate: real local social sanction exists against boundary-crossers, but it is discontinuous across localities and contested by ongoing status-mobility campaigns (Sanskritization-type reclassification), which is itself a form of resistance to boundary fixity. Theater ratio is low and only slowly rising (0.10 to 0.15) — under this reading there is little performative maintenance because the boundaries are doing genuine, continuously updated coordination work rather than defending an increasingly hollow claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Local jati councils and occupational guild clusters sit near the agenda-setting end — they administer and actively reshape the boundary. Kin networks and guild clusters are beneficiaries: the boundary reduces search and verification costs for marriage alliance, credit, and craft transmission. Boundary-crossing couples and locally low-ranked subgroups are targets: they bear locally enforced costs (ostracism, exclusion from commensal or occupational access) even though, under this reading, the boundary causing their exclusion is itself unstable and locally contingent rather than a fixed, uniformly-applied rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The practice reading resists a mandatrophy misreading in both directions: it does not claim the coordination function has fully atrophied into pure inertia (a piton reading would require a mostly-performative apparatus, which the low theater ratio here rejects), nor does it claim the boundary is doing nothing but extraction (a snare reading would require a stable enforcement apparatus, which the documented proliferation and instability argue against). The founding problem (legible coordination for marriage/trust/occupational transmission absent central registry) is authored as contested-live: substitute institutions (state registries, formal credit, civil marriage law) are eroding its relevance unevenly, which is exactly the kind of drift a rope classification should register rather than obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practice_vs_textual_boundary_locus,
    'Is the jati boundary a locally-negotiated practice convention (this reading) or an application, however imperfect, of a fixed scriptural varna framework that local practice merely instantiates with local noise (orthodox_textual_reading)?',
    'Comparative historical-anthropological analysis of whether documented jati splits/merges/renamings can be traced to textual reinterpretation (supporting textual reading) versus purely local socioeconomic pressure with no textual reference at all (supporting practice reading).',
    'If splits and renamings are shown to consistently invoke or negotiate against a textual varna referent, this reading''s low-extraction, weak-enforcement classification would need revision toward a more coercive textually-anchored account; if they show no such referent, the practice reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_vs_textual_boundary_locus, conceptual, 'Whether jati boundary dynamics are best explained by local practice autonomy or textual constraint.').

omega_variable(
    colonial_stabilization_confound,
    'How much of the observed ''proliferation'' this reading cites as evidence of local negotiability is itself an artifact of colonial census-taking practices that invited communities to register new, more advantageous jati names — i.e., is the proliferation evidence of pre-colonial local fluidity, or a colonial-era administrative side effect (colonial_census_reading''s domain)?',
    'Comparison of jati naming and category counts in pre-census ethnographic and administrative records against post-census enumeration data across the same regions.',
    'If most proliferation is post-census and coincides with enumeration incentives, this reading''s ε and coordination-dominance claim would need to be scoped to the post-colonial period only, weakening the claim that boundary fluidity is a longstanding, pre-existing feature of the practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_stabilization_confound, empirical, 'Whether documented category proliferation predates or is produced by colonial census apparatus.').

omega_variable(
    local_variance_vs_aggregate_extraction,
    'Does high local variance in enforcement intensity (this reading''s central claim) mask a stable aggregate extraction pattern when summed across all localities — i.e., is low extraction a real feature or an artifact of averaging highly variable local severities?',
    'Disaggregated locality-level extraction and enforcement-intensity data rather than aggregate/national-level estimates.',
    'If aggregate low extraction masks severe local extraction concentrated in specific regions or against specific low-status subgroups, the rope classification would be defensible only as a national-average artifact, and locality-specific readings might classify as tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(local_variance_vs_aggregate_extraction, empirical, 'Whether aggregate-level low extraction obscures locally concentrated high extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__localized_practice_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__localized_practice_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.23).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__localized_practice_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__localized_practice_reading, base_extractiveness, 100, 0.22).

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
% This story is one of three linked readings of the jati_practice_norm kernel. orthodox_textual_reading authors a fixed-scripture, high-suppression account; colonial_census_reading authors an administratively-stabilized, extraction-through-classification account; this file (localized_practice_reading) authors a low-extraction, high-fluidity coordination account. Each carries its own stable ε and its own beneficiary/victim structure per the ε-invariance principle; they are linked here rather than merged because the natural-language label 'jati boundaries' covers all three structurally distinct claims about what stabilizes the boundary and who profits from that stabilization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
