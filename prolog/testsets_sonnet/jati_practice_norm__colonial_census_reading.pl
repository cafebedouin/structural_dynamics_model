% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Reification of Jati Categories
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This story reads jati categorization through the specific lens of
 *   colonial administrative practice: the decennial census and accompanying
 *   ethnographic surveys converted a domain of locally negotiated,
 *   occupationally and maritally fluid group boundaries into a fixed, ranked,
 *   nationally legible schedule. The census apparatus needed countable
 *   categories for taxation, recruitment, and land settlement; ethnographers
 *   supplied a taxonomy modeled loosely on varna hierarchy and treated it as
 *   descriptive fact rather than administrative artifact. Communities that
 *   had previously moved between groupings, petitioned for local status
 *   recognition, or maintained ambiguous occupational identities found their
 *   position hardened into a durable written record that outlived the
 *   colonial state itself and became load-bearing infrastructure for
 *   post-colonial quota systems. This is a distinct constraint from the
 *   orthodox textual reading (which locates jati's authority in scripture and
 *   treats deviation as ritual pollution, not administrative inconvenience)
 *   and from the localized practice reading (which treats jati boundaries as
 *   continuously renegotiated coordination norms with no external freezing
 *   mechanism at all). The census reading's ε is moderate and tangled — a
 *   real governance-legibility function coexists with genuine asymmetric
 *   extraction imposed on communities who had no say in how they were
 *   classified.
 *
 * KEY AGENTS:
 *   - colonial_administrative_apparatus: agenda_setter (institutional/arbitrage) — designs and runs the schedule, insulated from consequences
 *   - colonial_ethnographers: agenda_setter/observer (institutional/analytical) — produce the taxonomy that becomes administrative fact
 *   - upper_caste_enumeration_beneficiaries: beneficiary (organized/mobile) — use codification to lock in status gains
 *   - fluid_boundary_communities: payer (powerless/trapped) — lose the mobility fixed classification forecloses
 *   - lower_status_jatis_reclassified_downward: payer (powerless/trapped) — hardened into a rank previously contestable
 *   - itinerant_and_occupational_groups: payer (powerless/constrained) — lose the boundary-crossing their livelihood required
 *   - post_colonial_successor_states: observer/beneficiary (institutional/constrained) — inherit and depend on the fixed registry for redistribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.58).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '6f0b6078-9bf5-4505-bb52-85029df7a05e').
narrative_ontology:cs_kernel_codification('6f0b6078-9bf5-4505-bb52-85029df7a05e', formalized).
narrative_ontology:cs_authority_grounding('6f0b6078-9bf5-4505-bb52-85029df7a05e', extraction).
narrative_ontology:cs_interpretation_layer_present('6f0b6078-9bf5-4505-bb52-85029df7a05e').
narrative_ontology:cs_reading_relation('6f0b6078-9bf5-4505-bb52-85029df7a05e', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f0b6078-9bf5-4505-bb52-85029df7a05e', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('6f0b6078-9bf5-4505-bb52-85029df7a05e', foundational, administrative_legibility_justifies_categorical_fixation).
narrative_ontology:cs_axiom_status(administrative_legibility_justifies_categorical_fixation, holdable).
narrative_ontology:cs_axiom_grounding('6f0b6078-9bf5-4505-bb52-85029df7a05e', administrative_legibility_justifies_categorical_fixation, instrumental).
narrative_ontology:cs_axiom('6f0b6078-9bf5-4505-bb52-85029df7a05e', foundational, external_enumeration_supersedes_local_negotiation_authority).
narrative_ontology:cs_axiom_status(external_enumeration_supersedes_local_negotiation_authority, holdable).
narrative_ontology:cs_axiom_grounding('6f0b6078-9bf5-4505-bb52-85029df7a05e', external_enumeration_supersedes_local_negotiation_authority, conventional).
narrative_ontology:cs_reference_frame('6f0b6078-9bf5-4505-bb52-85029df7a05e', precolonial_fluid_jati_negotiation).
narrative_ontology:cs_drift_state('6f0b6078-9bf5-4505-bb52-85029df7a05e', post_census_administrative_consolidation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('6f0b6078-9bf5-4505-bb52-85029df7a05e', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, upper_caste_enumeration_beneficiaries).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, fluid_boundary_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, lower_status_jatis_reclassified_downward).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, itinerant_and_occupational_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, post_colonial_successor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the decennial census apparatus, assigning each individual a fixed jati label pegged to a codified varna-ordered schedule. Uses the resulting registry for taxation, land settlement, army recruitment, and legal adjudication. Benefits from having a legible, countable population and is largely insulated from the on-the-ground consequences of miscategorization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Petition census commissioners and ethnographic surveyors to have their group's rank formally elevated in the schedules, using the fixed classification to lock in status gains that would have remained contestable under prior fluid practice. Once codified, the new rank becomes durable administrative fact, backed by state paperwork rather than local negotiation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, upper_caste_enumeration_beneficiaries, beneficiary,
    organized, generational, mobile, regional).

% Previously moved between occupational and marital-alliance groupings depending on local context, migration, or economic circumstance. Under the census schedule, their group is assigned a single fixed slot; the administrative act closes off the ambiguity and mobility that had let them shift affiliation as circumstances changed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, fluid_boundary_communities, payer,
    powerless, biographical, trapped, local).

% Placed by ethnographic surveyors into low positions on the schedule based on occupation or perceived ritual status, sometimes below where local practice had them. The written classification becomes the reference point for subsequent land rights, school admission quotas, and marriage negotiation, hardening a rank that had previously been argued over rather than fixed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, lower_status_jatis_reclassified_downward, payer,
    powerless, generational, trapped, regional).

% Groups whose livelihood depended on crossing jati lines (traders, performers, seasonal laborers) find the census apparatus forces a single fixed label onto what had been an occupationally negotiated, shifting identity, restricting the informal boundary-crossing their livelihoods required.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, itinerant_and_occupational_groups, payer,
    powerless, biographical, constrained, regional).

% Compile ethnographic handbooks and caste schedules, treating oral tradition and scattered practice as evidence for a single ranked taxonomy. Their scholarly output becomes the administrative reference document, converting contested and locally variable practice into a stable, citable hierarchy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_ethnographers, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, colonial_ethnographers, observer).

% Inherit the census-fixed jati categories as the basis for affirmative-action quota administration and legal recognition of scheduled groups. Depend on the same fixed registry for redistributive policy even while their legitimacy is grounded in redressing the very stratification the registry hardened.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, post_colonial_successor_states, observer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, post_colonial_successor_states, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides colonial (and later post-colonial) administration with a legible, countable population for taxation, recruitment, land settlement, and legal adjudication — a genuine governance problem that a state managing a subcontinental population needed some answer to.
% TRANSFER_FUNCTION: Moves interpretive control over jati boundaries from dispersed local negotiation (elders, marriage networks, occupational guilds) to a centralized administrative schedule; moves status gains to groups able to petition the census apparatus and status losses to groups whose local standing was overridden by ethnographic classification.
% ABSENT_VOICES: The communities whose boundaries were being fixed were not the authors of the schedules; their prior practice of continuous local renegotiation was treated as noise to be resolved rather than a legitimate coordination mechanism in its own right. Itinerant and occupationally mobile groups, whose entire structural position depended on ambiguity, had no seat in the ethnographic survey process.
% DISAPPEARANCE_RATIONALE: If the colonial census apparatus and its schedules disappeared, the administrative fixity of jati boundaries would lose its institutional anchor: quota systems, legal recognitions, and land records built on the fixed schedule would need re-derivation, and communities would regain latitude to renegotiate boundaries locally — though decades of path-dependent institutional reliance (especially post-colonial affirmative action architecture) mean the rearrangement would be contested and costly, not costless.
% FOUNDING_PROBLEM: A colonial state governing a vast, linguistically and socially heterogeneous population needed a legible, countable, administratively tractable way to assign taxation categories, military recruitment pools, and land tenure classes — existing local practice was too fluid and locally variable for centralized administration to process.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrative records and census commissioner reports attest the legibility problem was real and acute. Independent historians and anthropologists outside the administrative apparatus (working from oral history and pre-census marriage/occupation records) attest that local practice already had sufficient inter-group coordination mechanisms and that the fixed schedule solved an administrative convenience problem for the state, not a coordination failure among the communities themselves — the founding problem as stated by the administration is contested by scholarship examining the pre-census baseline.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58) because the constraint genuinely does solve a legibility problem for large-scale governance — this is not pure extraction dressed as coordination, it is a real administrative function riding alongside asymmetric costs imposed on classified communities. Suppression rose over the interval (0.35 to 0.62) as census categories were reinforced by subsequent legal, land, and recruitment systems that made the classification harder to contest with each cycle — enforcement hardened rather than the initial act alone doing the work. Theater ratio stays comparatively low (0.31) because the administrative function (taxation, recruitment, land records) remained genuinely load-bearing throughout, not merely performative, though its share of purely symbolic status-ranking activity grew as post-colonial identity politics increasingly used the same registry for purposes disconnected from its founding administrative rationale.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administrative apparatus's seat, the schedule is genuine coordination infrastructure enabling governance at scale. From the fluid-boundary and downward-reclassified communities' seats, the same schedule is an externally imposed freeze that closed off a previously available channel of local status negotiation. The engine should compute markedly different per-seat types from this identical structural data — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administrative apparatus and colonial ethnographers sit near the beneficiary end: they extract governance legibility and administrative control without bearing the classification's social costs, and their exit options (arbitrage/analytical) reflect their structural insulation. Upper-caste groups able to petition the census apparatus for elevated rank are secondary beneficiaries — they use the fixed system opportunistically. Fluid-boundary, downward-reclassified, and itinerant/occupational communities sit near the full-target end: trapped or constrained exit, no say in the classification, and the durable written record forecloses the local renegotiation channel they previously had. Post-colonial successor states occupy an ambiguous secondary position — they did not create the extraction but inherited and now depend on its infrastructure for redistributive purposes with different (though not unrelated) legitimacy claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding administrative-legibility problem (governing a heterogeneous population without a countable schedule) was arguably real at colonial founding but has been substantially superseded by modern administrative technology capable of handling fluid, self-reported, or periodically renegotiated categories. The classification persists not because the legibility problem remains unsolved by any other means, but because the fixed registry became load-bearing for quota systems whose own legitimacy now depends on the frozen categories continuing to exist — a mandatrophy risk where the original problem (colonial governance) is dead but a substitute justification (redistributive administration) sustains the frozen structure. This is precisely the seat-divergence the classification is built to catch: the administrative agenda_setter seat would compute this as ongoing coordination necessity, while the reclassified-community payer seat would compute it as inertial extraction whose founding rationale no longer applies to them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_kernel_reading_disambiguation,
    'Is the colonial-census freezing of jati categories best understood as an independent administrative innovation, or as a downstream amplification of a pre-existing orthodox textual varna schema that the census apparatus merely formalized?',
    'Comparative historical analysis of pre-census versus post-census jati boundary fluidity in regions with strong versus weak prior textual/Brahminical institutional presence — if fluidity was similar across both before the census and diverged only after, this reading''s independent-causation claim is strengthened; if regions with strong pre-existing textual authority already showed comparable rigidity, the orthodox_textual_reading''s claim of prior fixity gains support and this reading''s causal contribution is more modest.',
    'If the census apparatus merely formalized pre-existing textual rigidity, this reading''s extractiveness attribution to colonial administration should be revised downward and partially reattributed to the orthodox_textual_reading''s kernel; if the census apparatus is shown to have independently frozen categories that were genuinely fluid beforehand (as localized_practice_reading claims), this reading''s causal and extractive claims are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_kernel_reading_disambiguation, empirical, 'Whether administrative reification was an independent causal force or an amplifier of prior textual rigidity.').

omega_variable(
    post_colonial_inheritance_legitimacy,
    'Does the post-colonial state''s redistributive (quota-based) use of the same frozen categories constitute a genuine new coordination function that partially redeems the original extraction, or is it simply a second extraction riding on the first one''s infrastructure?',
    'Assess whether quota-based redistribution measurably improves outcomes for reclassified communities net of the ongoing costs of category rigidity (loss of local renegotiation channels, mismatch between fixed categories and lived social mobility) — longitudinal socioeconomic data compared against counterfactual flexible-classification systems where available.',
    'If redistributive use net-benefits the originally harmed communities, the post-colonial successor state''s beneficiary/observer dual role would shift toward genuine coordination, softening the tangled_rope classification for that seat specifically; if the frozen categories net-harm through mismatch and rigidity costs exceeding redistributive gains, the post-colonial extension would itself qualify as an additional tangled_rope layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_colonial_inheritance_legitimacy, empirical, 'Whether post-colonial redistributive reuse redeems or compounds the original administrative extraction.').

omega_variable(
    administrative_naturalization_risk,
    'To what extent has the census-fixed schedule become naturalized as ''how jati actually works'' even among scholars and communities, obscuring that it is one contingent administrative reading among several?',
    'Survey contemporary anthropological and sociological literature for uncritical reliance on census-era caste schedules as ground truth versus explicit acknowledgment of their constructed, administratively contingent origin.',
    'High naturalization would mean the tangled_rope classification here is itself contested by downstream scholarship that treats this reading''s categories as the orthodox_textual_reading''s or as simple natural fact — reinforcing the need for this story''s ε-invariant decomposition rather than treating ''jati'' as a single unproblematized construct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_naturalization_risk, conceptual, 'Whether the administrative reading has been mistaken for ground truth in subsequent scholarship and policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jati_tr_t15, jati_practice_norm__colonial_census_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__colonial_census_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(jati_tr_t45, jati_practice_norm__colonial_census_reading, theater_ratio, 45, 0.23).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__colonial_census_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(jati_tr_t75, jati_practice_norm__colonial_census_reading, theater_ratio, 75, 0.29).
narrative_ontology:measurement(jati_tr_t90, jati_practice_norm__colonial_census_reading, theater_ratio, 90, 0.31).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jati_be_t15, jati_practice_norm__colonial_census_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__colonial_census_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(jati_be_t45, jati_practice_norm__colonial_census_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__colonial_census_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(jati_be_t75, jati_practice_norm__colonial_census_reading, base_extractiveness, 75, 0.57).
narrative_ontology:measurement(jati_be_t90, jati_practice_norm__colonial_census_reading, base_extractiveness, 90, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jati_su_t15, jati_practice_norm__colonial_census_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__colonial_census_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(jati_su_t45, jati_practice_norm__colonial_census_reading, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__colonial_census_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(jati_su_t75, jati_practice_norm__colonial_census_reading, suppression_requirement, 75, 0.61).
narrative_ontology:measurement(jati_su_t90, jati_practice_norm__colonial_census_reading, suppression_requirement, 90, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.1).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the jati_practice_norm kernel, each with a distinct ε and stakeholder structure per the ε-invariance principle. colonial_census_reading (this story, tangled_rope, moderate ε ~0.58): external administrative reification for governance legibility, benefiting colonial/post-colonial administration at cost of frozen community boundaries. orthodox_textual_reading: grounds jati boundaries in fixed scriptural varna authority with deviation as ritual pollution — a distinct authority-grounding claim (textual/lineage rather than administrative/extraction). localized_practice_reading: treats jati boundaries as continuously renegotiated local coordination norms, denying that any durable external freezing occurred — likely lower ε, closer to rope, since its whole premise is that fluidity persisted and no asymmetric extraction structure crystallized. The three are linked, not merged; each should be independently classified by the engine from its own structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
