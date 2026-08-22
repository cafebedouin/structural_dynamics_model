% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Reading of Jati Boundaries as Fixed Varna Ordinance
 *   domain: social/religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates the orthodox_textual_reading of the
 *   contested kernel jati_practice_norm. It treats jati boundaries as
 *   divinely ordained, immutable derivatives of the fourfold varna schema
 *   codified in Dharmashastra texts (Manusmriti, Yajnavalkya Smriti).
 *   Deviation — inter-marriage, occupational crossing, commensality violation
 *   — is ritual pollution (ashaucha) requiring expiation or resulting in
 *   outcaste status. The authority structure (brahminical orthodoxy, dominant
 *   caste landholders, temple institutions) extracts labor, land surplus, and
 *   ritual service from avarna and shudra groups by enforcing categorical
 *   rigidity. The reading presents itself as Mountain (emerges_naturally:
 *   true in its own frame) but the authored metrics reveal high
 *   extractiveness, active enforcement, and near-total accessibility collapse
 *   for victims — the engine computes snare. The claim/metric divergence is
 *   deliberate: the reading's self-presentation as natural law is the cover
 *   story; the metrics describe the extraction machinery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.82).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.78).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Reading of Jati Boundaries as Fixed Varna Ordinance").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '40004a2f-5a8a-46fe-8169-377eda7f8940').
narrative_ontology:cs_kernel_codification('40004a2f-5a8a-46fe-8169-377eda7f8940', fixed_text).
narrative_ontology:cs_authority_grounding('40004a2f-5a8a-46fe-8169-377eda7f8940', lineage).
narrative_ontology:cs_interpretation_layer_present('40004a2f-5a8a-46fe-8169-377eda7f8940').
narrative_ontology:cs_reading_relation('40004a2f-5a8a-46fe-8169-377eda7f8940', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('40004a2f-5a8a-46fe-8169-377eda7f8940', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('40004a2f-5a8a-46fe-8169-377eda7f8940', foundational, varna_eternal_unchanging).
narrative_ontology:cs_axiom_status(varna_eternal_unchanging, holdable).
narrative_ontology:cs_axiom_grounding('40004a2f-5a8a-46fe-8169-377eda7f8940', varna_eternal_unchanging, deontological).
narrative_ontology:cs_axiom('40004a2f-5a8a-46fe-8169-377eda7f8940', foundational, ritual_pollution_binary_absolute).
narrative_ontology:cs_axiom_status(ritual_pollution_binary_absolute, holdable).
narrative_ontology:cs_axiom_grounding('40004a2f-5a8a-46fe-8169-377eda7f8940', ritual_pollution_binary_absolute, deontological).
narrative_ontology:cs_axiom('40004a2f-5a8a-46fe-8169-377eda7f8940', secondary, brahmin_authority_scriptural).
narrative_ontology:cs_axiom_status(brahmin_authority_scriptural, holdable).
narrative_ontology:cs_axiom_grounding('40004a2f-5a8a-46fe-8169-377eda7f8940', brahmin_authority_scriptural, theological).
narrative_ontology:cs_reference_frame('40004a2f-5a8a-46fe-8169-377eda7f8940', dharmashastra_varna_ideal).
narrative_ontology:cs_drift_state('40004a2f-5a8a-46fe-8169-377eda7f8940', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('40004a2f-5a8a-46fe-8169-377eda7f8940', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahminical_orthodoxy).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_caste_landholders).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, temple_institutions).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, avarna_dalit_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, shudra_occupational_groups).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, women_across_jati).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, shudra_occupational_groups).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_dharma_eternal).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_pollution_binary).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, scriptural_authority_supreme).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, transmits, and adjudicates the Dharmashastra textual tradition that defines varna-jati correspondence. Controls ritual calendar, temple appointments, and excommunication authority. Collects dakshina, land grants, and status rents. Does not bear the constraint's costs; the constraint constitutes their authority.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahminical_orthodoxy, agenda_setter,
    institutional, civilizational, analytical, continental).

% Hold land and political power in agrarian regions. Use jati hierarchy to command labor (begar, bonded labor, sharecropping) from lower jatis at below-market rates. Benefit from ritual hierarchy that legitimates their dominance. Can partially exit by migrating to urban/professional sectors while retaining land rents.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_caste_landholders, beneficiary,
    organized, generational, arbitrage, regional).

% Control temple land, endowments, and ritual economies. Enforce jati-based priesthood succession and ritual exclusivity. Collect offerings, land revenue, and state patronage. Constrained by state regulation (HRCE acts) but retain ritual authority as core asset.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, temple_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, temple_institutions, agenda_setter).

% Assigned polluting occupations (scavenging, tanning, cremation, agricultural labor) by jati prescription. Bear ritual pollution stigma that blocks access to water, temples, education, public space. Forced labor and sexual exploitation enforced by pollution logic. Exit (conversion, migration, education) is met with violence and continued stigma — jati is constitutive of social identity, not a voluntary association.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, avarna_dalit_communities, payer,
    powerless, biographical, identity_locked, local).

% Hereditary artisanal/service jatis (potters, weavers, barbers, washermen). Jati provides craft monopoly and mutual aid (coordination benefit) but extracts surplus upward to dominant castes and brahminical ritual economy. Occupational exit is blocked by pollution logic and loss of jati network. Some upward mobility via Sanskritization (adopting higher-jati customs) but within the varna frame, not outside it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, shudra_occupational_groups, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, shudra_occupational_groups, beneficiary).

% Jati endogamy is enforced through control of women's sexuality and marriage. Widowhood, menstruation, and childbirth are pollution events that intensify extraction. Women bear the reproductive labor of jati reproduction (endogamous marriage, ritual observance) with no authority over the norms. Exit is structurally blocked: inter-caste marriage invites honor violence; religious conversion severs kinship but not always stigma.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, women_across_jati, payer,
    powerless, biographical, identity_locked, local).

% Ambedkarite, Periyarist, Bhakti, and contemporary Dalit-Bahujan formations that reject varna-jati as theological fraud and extraction machinery. Would object to the orthodox reading's claim of natural law. Excluded from the orthodoxy's interpretive circle; their counter-readings are coded as adharma. Their political presence creates external pressure but does not enter the reading's internal logic.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, anti_caste_movements, excluded,
    organized, biographical, constrained, national).

% Academic philology, anthropology, history analyzing Dharmashastra texts, inscriptional records, and ethnographic data. Neither collects nor pays the constraint's extraction. Provides external evidence on textual layers, historical variation, and the gap between prescription and practice. Their analyses are cited by all parties but carry no authority within the orthodox reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, indological_scholarship, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a stratified division of labor and ritual status in agrarian society: each jati has a prescribed occupation, mutual obligations, and ritual rank. Solves the problem of allocating hereditary skills, labor, and ritual purity across a large population without market mechanisms.
% TRANSFER_FUNCTION: Moves labor (begar, bonded labor, low-wage agricultural work), agricultural surplus (sharecropper's produce), ritual fees (dakshina, temple offerings), and sexual/reproductive labor (endogamous marriage, control of women's sexuality) from avarna/shudra/women to brahminical_orthodoxy, dominant_caste_landholders, and temple_institutions.
% ABSENT_VOICES: Avarna and Dalit communities who would reject the varna framework entirely are structurally excluded from the orthodoxy's interpretive authority. Their voices appear only as 'pollution' or 'heresy' within the reading. The localized_practice_reading and colonial_census_reading represent alternative frameworks held by different parties (local powerholders, colonial state) that are not in conversation with the orthodox reading.
% DISAPPEARANCE_RATIONALE: If the orthodox textual constraint vanished overnight, the ritual-legal architecture justifying hereditary occupational bondage, pollution stigma, and endogamy enforcement would collapse. Agrarian labor relations would reorganize (toward market or new coercive forms), temple economies would lose ritual monopoly, and anti-caste movements would gain theological legitimacy. The material extraction machinery would lose its cover story.
% FOUNDING_PROBLEM: Post-Vedic agrarian expansion required a stable, hereditarily transmitted division of labor and surplus extraction mechanism that could integrate diverse tribal/kinship groups into a single social order without centralized bureaucracy. The varna-jati framework provided a theological template for this: ritual hierarchy legitimated material extraction, and endogamy preserved occupational castes.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociology (Romila Thapar, Suvira Jaiswal, Nicholas Dirks) and anti-caste genealogy (Ambedkar, Phule) attest the founding problem (agrarian surplus extraction via ritual hierarchy) is dead — the material base (landlordism, hereditary craft monopolies) has been transformed by colonialism, capitalism, and state formation. The orthodoxy (Shankaracharya mathas, VHP, RSS) attests the problem is live (dharma is eternal). No source outside the beneficiary set corroborates 'live'.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint channels material surplus (labor, produce, temple revenue) and symbolic capital (ritual status) from lower jatis to beneficiaries with no reciprocal obligation. Suppression (0.78) is high because the constraint is maintained by active social sanctions (excommunication, violence, economic boycott) and legal-administrative backing (colonial and postcolonial personal law). Theater_ratio (0.35) is moderate: the ritual-purity discourse performs theological coherence but the material extraction is the operative engine. Accessibility_collapse (0.88) is near-maximum for victims: occupational and marital exit is structurally blocked by the pollution logic itself — leaving the jati means becoming polluted, which is social death. Resistance (0.12) is low in the orthodox reading's frame because resistance is defined as adharma; measured resistance (Bhakti movements, anti-caste movements, Dalit assertion) is coded as heresy, not legitimate contestation, keeping the metric low within the reading's own epistemic closure.
 *
 * PERSPECTIVAL GAP:
 *   The brahminical_orthodoxy seat experiences this as Mountain (dharma, natural law). The avarna_dalit_communities seat experiences it as Snare (enforced extraction with blocked exit). The shudra_occupational_groups seat experiences it as Tangled Rope (coordinated artisanal production within jati but extraction upward). The engine computes this divergence from the structural data — the authored claim (snare) is the generating model's structural judgment, not any seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical_orthodoxy and temple_institutions are structural beneficiaries (collect ritual authority, material offerings, land grants — d near 0.0). Dominant_caste_landholders are beneficiaries of labor extraction and land control (d ~0.15). Avarna_dalit_communities are full targets (bear pollution stigma, forced labor, landlessness, violence — d ~0.95). Shudra_occupational_groups are targets with partial coordination benefit (jati provides craft monopoly but extracts surplus upward — d ~0.75). Women_across_jati are targets with gendered intensification (purity/pollution logic regulates sexuality, marriage, widowhood — d ~0.85). Exit_options are identity_locked for all victim seats: jati is constitutive of self, not a contract one can exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (varna_order_maintenance) is coded as 'live' by the reading's authority structure but 'dead' by historical sociology — the material conditions (agrarian surplus extraction via ritual hierarchy) that made varna functional are gone, yet the constraint persists with intensified extraction. This is classic mandatrophy: the coordination function (social order via ritual hierarchy) has atrophied; what remains is extraction machinery defended by theological cover. The reading prevents mislabeling by declaring itself Mountain while the metrics expose Snare — the divergence is the measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_fixity_vs_historical_stratification,
    'Does the varna schema in Dharmashastra texts describe an observed social reality or prescribe an ideal order that was retrojected as eternal?',
    'Textual-historical analysis of Dharmashastra composition layers vs. archaeological/inscriptional evidence of actual jati-varna mapping in precolonial periods.',
    'If prescriptive/retrojective, the ''fixed scriptural framework'' claim is a constructed natural law — the constraint is a false summit (mountain claim with beneficiaries) and FSM triggers reclassification to tangled_rope or snare. If descriptive, the mountain claim holds structurally (though extraction metrics would still compute).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_fixity_vs_historical_stratification, conceptual, 'Whether the textual varna framework is a discovered natural kind or an authored ideological projection.').

omega_variable(
    pollution_mechanism_structural_vs_internalized,
    'Is the suppression of jati deviation maintained by external structural barriers (violence, boycott, law) or by internalized pollution belief (the target believes crossing boundaries makes them ritually impure)?',
    'Post-exit suppression trajectory: track individuals/groups who exit jati norms (conversion, migration, intermarriage) — if pollution stigma and material penalty persist after exit, suppression is structural; if stigma dissolves but material penalty remains, internalized belief was the carrier.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression mechanism internally. This affects directionality derivation for identity_locked agents and the theta amplification in the engine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pollution_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism for jati boundary enforcement.').

omega_variable(
    reading_relations_kernel_jati_practice_norm,
    'What is the structural relationship between this orthodox_textual_reading and its sibling readings (localized_practice_reading, colonial_census_reading) of the jati_practice_norm kernel?',
    'Map the logical space: does the orthodox claim of scriptural fixity logically foreclose the localized claim of continuous renegotiation? Does the colonial census reading''s external reification influence (create downstream pressure on) the orthodox reading''s authority claims?',
    'Determines cs_structure.reading_relations values (forecloses/coexists_with/influences) which feed the kernel drift engine. A forecloses relation means the readings cannot coexist in one framework; coexists_with means they are held by different parties simultaneously; influences means structural pressure without logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relations_kernel_jati_practice_norm, conceptual, 'Structural relations among sibling readings of the jati_practice_norm kernel.').

omega_variable(
    axiom_grounding_type_empirical_contingency,
    'Are the foundational axioms of this reading (varna_eternal, pollution_binary) grounded in empirically contingent claims (e.g., ''society collapses without varna'') that could be falsified, or in deontological claims (intrinsic ritual status) that are not?',
    'Analyze the reading''s own textual corpus: when challenged (e.g., by Bhakti, Buddhist, or modern reform movements), does the orthodoxy defend varna on consequentialist grounds (social order) or intrinsic grounds (dharma)? Consequentialist defenses route to empirically_contingent; intrinsic defenses route to deontological.',
    'If axioms are empirically_contingent and drift_state.direction = axiom_overriding with magnitude substantial/severe, the engine computes foreclosure risk. If deontological, foreclosure does not trigger regardless of drift. This determines whether the reading''s authority structure is brittle to empirical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_grounding_type_empirical_contingency, conceptual, 'Epistemic grounding type of the reading''s distinguishing axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 1500, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_orthodox_textual_tr_t1500, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(jati_orthodox_textual_tr_t1650, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1650, 0.22).
narrative_ontology:measurement(jati_orthodox_textual_tr_t1800, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(jati_orthodox_textual_tr_t1850, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1850, 0.45).
narrative_ontology:measurement(jati_orthodox_textual_tr_t1900, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(jati_orthodox_textual_tr_t1950, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(jati_orthodox_textual_tr_t2000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(jati_orthodox_textual_tr_t2024, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(jati_orthodox_textual_be_t1500, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(jati_orthodox_textual_be_t1650, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1650, 0.72).
narrative_ontology:measurement(jati_orthodox_textual_be_t1800, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1800, 0.78).
narrative_ontology:measurement(jati_orthodox_textual_be_t1850, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1850, 0.85).
narrative_ontology:measurement(jati_orthodox_textual_be_t1900, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(jati_orthodox_textual_be_t1950, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1950, 0.75).
narrative_ontology:measurement(jati_orthodox_textual_be_t2000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(jati_orthodox_textual_be_t2024, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jati_orthodox_textual_su_t1500, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(jati_orthodox_textual_su_t1650, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1650, 0.68).
narrative_ontology:measurement(jati_orthodox_textual_su_t1800, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(jati_orthodox_textual_su_t1850, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1850, 0.85).
narrative_ontology:measurement(jati_orthodox_textual_su_t1900, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(jati_orthodox_textual_su_t1950, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(jati_orthodox_textual_su_t2000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(jati_orthodox_textual_su_t2024, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, manual_scavenging_practice).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, temple_entry_movement).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, reservation_policy_constitutional).

% DUAL FORMULATION NOTE:
% This is the orthodox_textual_reading of the jati_practice_norm kernel. The sibling readings are localized_practice_reading (jati as fluid local coordination) and colonial_census_reading (jati as administrative reification). This reading claims scriptural fixity and ritual pollution enforcement (high ε, snare); the localized reading claims negotiation and proliferation (lower ε, rope/tangled_rope); the colonial reading claims external fixation for governance (medium ε, scaffold/tangled_rope). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, institutional, 0.05).
constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, organized, 0.15).
constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, powerless, 0.95).
constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, moderate, 0.75).
constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
