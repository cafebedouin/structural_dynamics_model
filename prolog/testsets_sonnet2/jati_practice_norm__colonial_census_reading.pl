% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Fixation of Jati Categories
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This story instantiates the colonial-census reading of the
 *   jati_practice_norm kernel: the claim that jati categories were stabilized
 *   and reified through an external administrative apparatus — the decennial
 *   census and its ethnographic schedules — built for governance legibility
 *   across a territory too large to govern through local knowledge alone.
 *   Under this reading, jati was not a fixed scriptural category (the
 *   orthodox_textual_reading) nor a continuously renegotiated local
 *   coordination norm (the localized_practice_reading), but a historically
 *   fluid social field that an external bureaucracy froze into discrete,
 *   enumerable, administratively actionable units. The freezing served a real
 *   coordination function (tax assessment, legal adjudication, administrative
 *   recruitment at scale) while simultaneously transferring the power to
 *   define status away from local, contestable, and mobile arrangements
 *   toward a written registry that outlived its original administrative
 *   purpose and was inherited by postcolonial law as the basis for
 *   reservation policy.
 *
 * KEY AGENTS:
 *   - colonial_administration: primary agenda-setter, designs and enforces the fixed schedule for tax and legal purposes
 *   - census_enumeration_bureaucracy: administers and professionally depends on the schedule's continued authority
 *   - dominant_caste_associations: petition the fixed schedule to lock in favorable status, benefiting from its rigidity
 *   - boundary_communities, itinerant_occupational_groups, intermarrying_lineages: bear the cost of foreclosed fluidity
 *   - local_religious_authorities: bypassed customary arbiters, excluded from schedule design
 *   - postcolonial_courts_and_legislatures: analytical/observer seat inheriting and perpetuating the fixed framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.58).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Fixation of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, 'fdbc2dd0-ced9-4c09-b37b-859183abd5ea').
narrative_ontology:cs_kernel_codification('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', distributed).
narrative_ontology:cs_authority_grounding('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', extraction).
narrative_ontology:cs_interpretation_layer_present('fdbc2dd0-ced9-4c09-b37b-859183abd5ea').
narrative_ontology:cs_reading_relation('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', foundational, administrative_legibility_justifies_categorical_fixation).
narrative_ontology:cs_axiom_status(administrative_legibility_justifies_categorical_fixation, holdable).
narrative_ontology:cs_axiom_grounding('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', administrative_legibility_justifies_categorical_fixation, instrumental).
narrative_ontology:cs_axiom('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', secondary, written_registry_supersedes_customary_status_determination).
narrative_ontology:cs_axiom_status(written_registry_supersedes_customary_status_determination, holdable).
narrative_ontology:cs_axiom_grounding('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', written_registry_supersedes_customary_status_determination, conventional).
narrative_ontology:cs_reference_frame('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', precolonial_fluid_status_field).
narrative_ontology:cs_drift_state('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', postcolonial_reservation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fdbc2dd0-ced9-4c09-b37b-859183abd5ea', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, dominant_caste_associations).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, census_enumeration_bureaucracy).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, boundary_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, itinerant_occupational_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, intermarrying_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the decennial census apparatus, assigning each household a fixed jati label drawn from a standardized schedule. Uses the resulting registry to allocate administrative posts, set land-revenue categories, and adjudicate legal disputes over status. Justifies the fixed schedule as necessary for governance legibility, tax assessment, and law-and-order planning across a population too large to govern through case-by-case local knowledge.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, analytical, continental).

% Enumerators, ethnographic surveyors, and record clerks build careers around producing and defending the caste schedules. They collect fees, patronage, and professional standing from being the recognized arbiters of who belongs to which category, and from adjudicating petitions for reclassification.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, census_enumeration_bureaucracy, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, census_enumeration_bureaucracy, beneficiary).

% Petition the census apparatus to fix favorable rankings in the official schedule, using the newly rigid categories to lock in claims to higher ritual status, land grants, and access to government employment quotas tied to caste identity. Their exit options are wide: they can lobby the same apparatus that constrains others to their advantage.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, dominant_caste_associations, beneficiary,
    organized, generational, mobile, national).

% Groups whose status was historically ambiguous or locally negotiated find themselves assigned to a single administrative category that forecloses prior fluidity. Loss of access to occupations, marriage alliances, or ritual roles previously available under looser local arrangements. Petitioning for reclassification requires resources and literacy most do not have; the census schedule becomes the reference point courts and employers use against them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, boundary_communities, payer,
    powerless, generational, trapped, regional).

% Groups whose livelihoods depended on crossing between locally recognized occupational categories are forced into a single fixed administrative slot, criminalizing or stigmatizing prior mobility (some reclassified wholesale as 'criminal tribes' under the same administrative logic). Their traditional flexibility becomes administratively illegible and therefore penalized.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, itinerant_occupational_groups, payer,
    powerless, biographical, trapped, regional).

% Families whose status derived from locally negotiated intermarriage patterns are forced to declare a single jati for census purposes, severing recognition of mixed or intermediate status that previously functioned as a live social category. The written record becomes authoritative over lived practice in subsequent disputes.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, intermarrying_lineages, payer,
    powerless, generational, trapped, local).

% Priests and local arbiters who previously adjudicated status disputes through custom and negotiation are bypassed by the census schedule, which supersedes their determinations in matters of state recognition, employment, and law. They are not consulted in schedule design and have no formal channel to contest classifications once fixed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_religious_authorities, excluded,
    moderate, generational, constrained, local).

% Inherit the colonial-era caste schedules as the evidentiary basis for later reservation and affirmative-action policy, and periodically hear petitions to add, remove, or reclassify jatis within the inherited framework, effectively perpetuating the fixed-category logic under new administrative purposes.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single standardized registry that lets a distant administrative apparatus assess taxes, allocate offices, and adjudicate status disputes across a population far too large and diverse for case-by-case local knowledge to scale — a genuine legibility problem for large-territory governance.
% TRANSFER_FUNCTION: Moves the power to define social status from distributed local negotiation (priests, elders, marriage networks, occupational guilds) to a centralized enumeration bureaucracy; moves practical flexibility and reclassification opportunity away from ambiguous or intermediate groups and toward groups positioned to lobby the schedule in their favor.
% ABSENT_VOICES: Local religious and customary authorities who previously arbitrated status disputes are not consulted in schedule design. Boundary communities and itinerant groups whose historical status was fluid have no seat in the classification process and discover the fixed category only when it is applied against them in a land, employment, or marriage dispute.
% DISAPPEARANCE_RATIONALE: If the administratively fixed schedule vanished, status determination would revert to distributed local negotiation and customary arbitration; land-revenue records, employment quota systems, and legal precedent built on the fixed categories would require wholesale reconstruction, and communities currently locked into unfavorable classifications would regain room to renegotiate standing through marriage, occupation change, or migration.
% FOUNDING_PROBLEM: A distant colonial administration needed a legible, low-cost way to assess land revenue, recruit labor and military personnel, and adjudicate status-based legal disputes across a vast and locally heterogeneous population without relying on case-by-case local knowledge it did not possess.
% FOUNDING_PROBLEM_CORROBORATION: Postcolonial historians and legal scholars outside the beneficiary groups (e.g. work tracing the shift from fluid jati practice to census-fixed categories) attest that the original administrative-efficiency problem the schedule solved ceased with the colonial state's departure; the schedule persists because it was inherited wholesale as the evidentiary basis for post-independence reservation policy and land records, not because the original legibility problem still exists in its original form. No corroboration for continued necessity comes from outside the bureaucratic and dominant-caste beneficiaries who now depend on the fixed schedule for quota and land claims.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58 at interval end) — real administrative coordination value exists (tax assessment, legal legibility at scale), but a substantial share of the fixing's cost falls on groups whose prior fluidity is criminalized or erased, and on communities who lose bargaining leverage once their status is reduced to a single line in a government schedule. Suppression (0.62) reflects that the schedule's persistence depends on active enforcement — legal precedent, land records, and quota administration that treat the fixed category as authoritative over lived practice, backed by the state's power to adjudicate disputes using the schedule rather than local custom. Theater ratio rises over the interval (0.15 to 0.40) as the administrative-efficiency justification becomes decreasingly load-bearing (the colonial state that needed it is gone) while the schedule's institutional apparatus (courts, quota administration, caste certificates) persists and expands its own procedural elaboration.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial and successor bureaucratic administration sit at the beneficiary end: they gain governance legibility and the schedule collects institutional continuity and professional stakes for enumerators and record-keepers. Dominant caste associations are secondary beneficiaries — organized, mobile, able to lobby the same apparatus that constrains others. Boundary communities, itinerant occupational groups, and intermarrying lineages sit at the target end: powerless, trapped by the schedule's authority over land, employment, and marriage-status disputes, with no comparable capacity to petition for favorable reclassification. Local religious authorities are excluded rather than coordinated or extracted from directly — their institutional role is simply superseded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial administrative legibility) is dead, but the schedule survives because postcolonial law inherited it wholesale as the evidentiary basis for reservation policy — a genealogical mismatch (founding_problem_status: dead, disappearance_verdict: world_rearranges) that signals the classic zombie-mandate pattern: the original coordination justification has expired, but removing the arrangement now would still rearrange the world because so much subsequent law and social expectation has been built on top of it. This is precisely why tangled_rope (not snare) is the correct claim: a genuine coordination function existed and initially justified the mechanism, but the mechanism now persists through institutional inertia and vested interest rather than through the coordination problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_colonial_administrative,
    'Is the colonial-census reading (administrative fixation for governance legibility) the correct structural account of jati stabilization, or do the orthodox_textual_reading (scriptural varna framework) and localized_practice_reading (continuous local renegotiation) better capture the mechanism in different regions or periods?',
    'Comparative historical and anthropological evidence: regions/periods where census enumeration demonstrably altered pre-existing status practice (recorded petitions for reclassification, documented category proliferation before vs. after enumeration) support this reading; regions where textual/ritual authorities or local negotiation dominated with minimal administrative interference would support the sibling readings instead.',
    'If the localized_practice_reading is empirically dominant in a given region, the colonial administration''s role was closer to recording pre-existing fluidity than freezing it, which would lower this reading''s claimed extractiveness and shift victim attribution; if the orthodox_textual_reading dominates, the administrative apparatus would be better read as an enforcement layer atop a pre-existing scriptural hierarchy rather than as the primary fixing mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_colonial_administrative, conceptual, 'Which kernel reading (administrative, textual, or practice-based) best fits a given historical/regional case is contested and not resolvable from this story alone.').

omega_variable(
    administrative_efficiency_vs_extraction_intent,
    'Was the fixing of categories primarily an unintended byproduct of administrative efficiency-seeking, or did colonial administrators deliberately exploit caste fixation as a divide-and-rule extraction strategy?',
    'Archival analysis of internal administrative correspondence and census commissioner reports for explicit statements of intent versus purely technical/statistical justification.',
    'Deliberate extraction intent would push this reading toward snare-like characterization for the colonial period specifically; efficiency-driven unintended reification supports the tangled_rope reading as authored, where genuine coordination function coexists with asymmetric cost distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_efficiency_vs_extraction_intent, empirical, 'Whether administrative fixation was intentional extraction strategy or unintended coordination byproduct.').

omega_variable(
    postcolonial_inheritance_legitimacy,
    'Does the postcolonial state''s continued use of colonial-era caste schedules for reservation policy constitute a legitimate repurposing (new coordination function: redressing historical disadvantage) or a mere continuation of the original extractive fixation under new administrative cover?',
    'Analysis of whether reservation-policy outcomes track the schedule''s original administrative categories faithfully or have been substantially revised through independent evidence of social disadvantage, decoupled from the colonial schedule''s specific boundaries.',
    'If reservation policy has substantially decoupled from the original colonial schedule (independent disadvantage assessment), the founding_problem_status assessment of ''dead'' would need revision toward ''contested'' with a new, independently justified coordination function; if it remains schedule-dependent, the zombie-mandate reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(postcolonial_inheritance_legitimacy, preference, 'Whether postcolonial repurposing of the schedule constitutes new legitimate coordination or continued extraction under new justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__colonial_census_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__colonial_census_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__colonial_census_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__colonial_census_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__colonial_census_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__colonial_census_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__colonial_census_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__colonial_census_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__colonial_census_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__colonial_census_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__colonial_census_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__colonial_census_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__colonial_census_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__colonial_census_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__colonial_census_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'jati categories' per the ε-invariance principle. Each reading of the jati_practice_norm kernel names a structurally distinct mechanism with its own ε: colonial_census_reading (this story, ε=0.58, tangled_rope, administrative fixation) is linked to orthodox_textual_reading (scriptural varna framework, ritual pollution logic) and localized_practice_reading (continuous local renegotiation, likely lower extraction / rope-like). The three do not average into one ε; they are separate constraints connected by shared kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
