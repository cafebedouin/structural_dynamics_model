% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Monopoly on Ritual/Interpretive Authority (Varna as Divine Ordinance)
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This story instantiates the hereditary-monopoly reading of the
 *   Vedic-dharmic textual kernel: ritual and interpretive authority is held
 *   to derive from birth into Brahmin lineage, and varna hierarchy is read as
 *   divinely ordained and textually prescribed rather than historically
 *   contingent. This is NOT the only defensible reading of the shared corpus
 *   — the bhakti devotional reading and the reformist egalitarian reading are
 *   separate constraints instantiating different structural claims from the
 *   same textual kernel, linked here via network edges rather than folded
 *   into this story's classification. Under this reading specifically, the
 *   coordination function (reliable transmission of a demanding oral corpus)
 *   is real but has been layered with an asymmetric extraction structure
 *   (birth-gated collection of ritual fees, land endowments, and social
 *   deference) that requires active institutional enforcement (temple access
 *   restriction, exclusion from initiation and study) to persist.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects ritual and interpretive rents on the basis of birth
 *   - temple_administrative_institutions: Secondary beneficiary (institutional/arbitrage) — routes ritual economy revenue through the hereditary claim
 *   - shudra_and_dalit_communities: Primary target (powerless/trapped) — bears exclusion from ritual and interpretive standing
 *   - women_across_varnas: Cross-cutting target (powerless/constrained) — bears gender-layered exclusion regardless of birth varna
 *   - non_brahmin_ritual_practitioners: Secondary target (moderate/constrained) — contested legitimacy despite ritual competence
 *   - colonial_and_postcolonial_state_authorities: Analytical observer (institutional/analytical) — produces external documentary record
 *   - reform_and_devotional_movements: Excluded voice (organized/constrained) — objects from within the tradition, excluded from this reading's account of legitimate interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.78).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Monopoly on Ritual/Interpretive Authority (Varna as Divine Ordinance)").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '7577e921-324b-47bf-91e9-23b06b9c43d9').
narrative_ontology:cs_kernel_codification('7577e921-324b-47bf-91e9-23b06b9c43d9', fixed_text).
narrative_ontology:cs_authority_grounding('7577e921-324b-47bf-91e9-23b06b9c43d9', lineage).
narrative_ontology:cs_interpretation_layer_present('7577e921-324b-47bf-91e9-23b06b9c43d9').
narrative_ontology:cs_reading_relation('7577e921-324b-47bf-91e9-23b06b9c43d9', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('7577e921-324b-47bf-91e9-23b06b9c43d9', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('7577e921-324b-47bf-91e9-23b06b9c43d9', foundational, ritual_competence_is_birth_determined).
narrative_ontology:cs_axiom_status(ritual_competence_is_birth_determined, holdable).
narrative_ontology:cs_axiom_grounding('7577e921-324b-47bf-91e9-23b06b9c43d9', ritual_competence_is_birth_determined, theological).
narrative_ontology:cs_axiom('7577e921-324b-47bf-91e9-23b06b9c43d9', foundational, varna_hierarchy_is_scripturally_essential_not_accretive).
narrative_ontology:cs_axiom_status(varna_hierarchy_is_scripturally_essential_not_accretive, holdable).
narrative_ontology:cs_axiom_grounding('7577e921-324b-47bf-91e9-23b06b9c43d9', varna_hierarchy_is_scripturally_essential_not_accretive, conventional).
narrative_ontology:cs_reference_frame('7577e921-324b-47bf-91e9-23b06b9c43d9', brahminical_textual_orthodoxy).
narrative_ontology:cs_drift_state('7577e921-324b-47bf-91e9-23b06b9c43d9', post_constitutional_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7577e921-324b-47bf-91e9-23b06b9c43d9', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_and_dalit_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_ritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Sanskrit textual transmission, temple ritual performance, and the interpretive gatekeeping that determines which readings of dharmic texts count as authoritative. Collects patronage, land grants, ritual fees, and social deference tied directly to birth status rather than demonstrated learning. Can move between regions and institutions without losing standing because the lineage credential travels with birth, not location.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, beneficiary).

% Administers endowed land, ritual economy, and pilgrimage revenue that is structurally routed through Brahmin ritual monopoly. Depends on the hereditary authority claim for its own institutional legitimacy and revenue base; has no incentive to certify non-Brahmin ritual competence.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_institutions, beneficiary,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_institutions, agenda_setter).

% Historically and in many regions presently excluded from temple entry, textual study, and independent ritual officiation. Bears the cost of the hierarchy through denied access to religious capital, social exclusion, and economic disadvantage tied to caste designation. Exit requires either conversion out of the tradition entirely or reform movements operating against active resistance from incumbent authorities.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_and_dalit_communities, payer,
    powerless, generational, trapped, national).

% Excluded from Vedic study, upanayana initiation, and most ritual officiation regardless of birth varna, under textual readings that treat gender as a further ritual disqualification layered onto caste. Bears reduced inheritance, education, and interpretive standing as a structural cost of the hierarchy's operation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas, payer,
    powerless, generational, constrained, national).

% Individuals or communities (e.g. non-Brahmin priests in some regional traditions) who perform ritual functions but lack the birth credential this reading treats as constitutive of legitimate authority. Face contested legitimacy, reduced patronage, and periodic exclusion from mainstream temple networks even when ritually knowledgeable.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, non_brahmin_ritual_practitioners, payer,
    moderate, biographical, constrained, regional).

% Codified, litigated, and periodically restructured caste-based temple access and personal law through legislation and court rulings, without being a party that collects ritual authority itself. Produces the documentary record (census categories, temple-entry legislation, court testimony) used to assess the hierarchy's operation from outside the tradition's own institutions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, colonial_and_postcolonial_state_authorities, observer,
    institutional, generational, analytical, national).

% Bhakti saints, anti-caste reformers, and constitutional-equality advocates have long argued that birth-based ritual authority is not scripturally essential. Under this reading their claims are treated as heterodox innovations rather than valid interpretations, so their objections are structurally present in the historical record but excluded from the reading's own account of legitimate authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reform_and_devotional_movements, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible system for preserving, correctly pronouncing, and ritually deploying a large and linguistically difficult textual corpus (Vedic Sanskrit) across many generations without a centralized institution — lineage-based training solves a genuine transmission-fidelity problem in a pre-print, pre-standardized-education context.
% TRANSFER_FUNCTION: Moves ritual fees, land endowments, temple administrative control, social deference, and interpretive authority from all other varnas (and from women within every varna) to Brahmin lineages, on the basis of birth rather than demonstrated competence or devotion.
% ABSENT_VOICES: Shudra, Dalit, and women's voices arguing for competence- or devotion-based access to ritual and interpretive authority are extensively documented in bhakti literature, anti-caste reform writing, and constitutional debate, but this reading's own framework treats such claims as external to legitimate textual interpretation rather than as competing readings of the same texts.
% DISAPPEARANCE_RATIONALE: If hereditary ritual monopoly disappeared, temple administration, ritual fee structures, marriage and inheritance practices tied to varna, and the social deference economy built around Brahmin lineage would all require reorganization around competence-, devotion-, or election-based alternatives — arrangements that already exist in parallel (bhakti networks, reform temples, some regional non-Brahmin priesthoods) and would likely absorb the displaced function.
% FOUNDING_PROBLEM: Reliable oral transmission of a vast, phonetically precise Vedic corpus across centuries, in a context with no writing system in wide ritual use and high stakes attached to correct recitation, required a dedicated, intensively trained class committed across generations to memorization and correct performance.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin lineages and temple institutions attest the founding transmission problem remains live and justifies continued hereditary authority. Independent textual historians, anti-caste scholars (e.g. the Dravidian and Ambedkarite reform traditions), and postcolonial courts attest that transmission is now achievable through written texts, recorded audio, and open pedagogical institutions, and that the birth requirement persists primarily as a rent-preserving social hierarchy rather than a transmission necessity; this corroboration comes from outside the beneficiary class.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.65, reflecting substantial but not maximal extraction: a genuine transmission-coordination function exists underneath the hierarchy, but historical and contemporary evidence (temple-entry restrictions, differential ritual fee structures, exclusion from Vedic study) shows asymmetric benefit concentrated in Brahmin lineages relative to cost borne by lower castes and women. Suppression (0.78) is high because the arrangement's persistence has historically depended on active enforcement — social sanction, temple access denial, legal codification of caste status under colonial administration — not merely on voluntary deference. Theater ratio rises over the measured interval (0.20 to 0.42) reflecting a documented pattern: as legal and constitutional pressure against caste-based exclusion increased (especially post-1950 in India), much surviving enforcement shifted from substantive gatekeeping toward performative assertions of tradition, ritual correctness, and textual authenticity, while actual material extraction (land control, direct ritual fee monopolies) somewhat receded — hence extractiveness peaks mid-interval and tapers slightly while theater keeps climbing.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin lineage seat, the arrangement is read as sacred transmission fidelity — the hierarchy simply reflects textually and cosmologically given differences in ritual competence, and objecting to it is objecting to dharma itself. From the shudra/dalit and women's seats, the identical arrangement operates as enforced exclusion from religious, economic, and social capital, justified after the fact by texts whose authorship and interpretation is itself controlled by the beneficiary class. The engine is expected to compute these as structurally different experiences of the same authored data — the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin lineages and temple institutions are declared beneficiaries with arbitrage-grade exit (their credential travels with birth across regions and institutions), driving d toward the beneficiary end. Shudra/Dalit communities are declared victims with trapped exit (caste status is not exitable within the tradition's own terms), driving d toward the full-target end. Women are declared victims with constrained (not fully trapped) exit, since some avenues (conversion, secular life, reform movements) exist but at high social cost. Non-Brahmin ritual practitioners occupy an intermediate position: moderate power, constrained exit, contested but not fully foreclosed legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable oral transmission of a difficult corpus) is contested as live: it was genuinely urgent in a pre-literate ritual context and is corroborated as largely resolved by external, non-beneficiary sources (textual historians, reform scholars, courts) given the availability of writing, audio recording, and open pedagogy. The reading's own account treats the founding problem as still live and constitutive, which is precisely the mismatch (status=contested tending toward dead-outside-the-tradition, verdict=world_rearranges) that the R5 apparatus is built to surface: an arrangement whose original coordination function has substantially diminished while its extraction and enforcement machinery persists is exactly the zombie-mandate pattern this framework flags for further scrutiny, distinct from either a clean Rope (function still fully live) or a clean Snare (no coordination function ever existed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordination_vs_constructed_hierarchy,
    'Is the varna hierarchy genuinely textually and cosmologically prescribed as this reading claims, or is the hereditary-monopoly interpretation itself a later, historically contingent overlay on a more textually ambiguous or contested corpus?',
    'Comparative philological analysis of the earliest strata of Vedic and dharmic texts against later smriti and commentarial literature, tracing where explicit hereditary-birth requirements for ritual authority first appear versus where competence- or devotion-based framings appear in the same textual tradition.',
    'If the hereditary claim is a later interpretive layer rather than an original textual commitment, this reading''s foundational premise (varna as divinely ordained and textually prescribed) is substantially weakened, supporting the reformist_egalitarian_reading''s historical-accretion account over this reading''s own self-understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordination_vs_constructed_hierarchy, conceptual, 'Whether the hereditary hierarchy claim is original to the corpus or a constructed later accretion.').

omega_variable(
    transmission_necessity_persistence,
    'Given that writing, recording, and open pedagogical institutions now exist, does the original transmission-fidelity coordination function still require birth-based restriction, or has the coordination function become fully separable from the hereditary gate?',
    'Comparative study of non-Brahmin-run Vedic study institutions and reform-movement ritual training programs: if recitation fidelity and ritual competence outcomes are comparable to lineage-trained practitioners, the functions are separable.',
    'If separable, the persisting hereditary restriction is best read as extraction riding on a now-optional coordination justification; if inseparable, some portion of the measured extraction reflects a genuine residual coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_necessity_persistence, empirical, 'Whether the hereditary transmission requirement remains functionally necessary or has become severable extraction.').

omega_variable(
    reading_selection_framing,
    'Is the hereditary-monopoly reading the historically dominant or textually primary reading of the kernel, or is its dominance itself an artifact of which interpretive institutions (Brahmin-controlled commentarial traditions) had the power to canonize their own reading over bhakti and egalitarian alternatives?',
    'Historical mapping of which reading held institutional and state power at different periods (pre-bhakti classical period, bhakti movement era 8th-17th century CE, colonial codification, postcolonial constitutional era) and whether canonization tracked textual argument or institutional power.',
    'If canonization tracked institutional power rather than textual argument, this reading''s claim to textual primacy is itself part of the extraction mechanism (interpretive authority naturalizing its own historical construction) rather than a neutral description of the corpus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_framing, conceptual, 'Whether this reading''s apparent textual primacy reflects genuine textual weight or the historical power of its own interpretive institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(vedi_tr_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement(vedi_tr_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 160, 0.42).
narrative_ontology:measurement(vedi_tr_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(vedi_be_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 120, 0.7).
narrative_ontology:measurement(vedi_be_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 160, 0.68).
narrative_ontology:measurement(vedi_be_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(vedi_su_t120, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 120, 0.8).
narrative_ontology:measurement(vedi_su_t160, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 160, 0.79).
narrative_ontology:measurement(vedi_su_t200, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_dharmic_corpus kernel. bhakti_devotional_reading holds that sincere devotion bypasses caste-based ritual gatekeeping (lower ε, different beneficiary/victim structure: devotional communities as beneficiaries, orthodox ritual establishment's monopoly as the thing bypassed). reformist_egalitarian_reading holds that textual meaning must conform to constitutional equality and caste hierarchy is historical accretion (lowest ε for the standing arrangement as this reading would describe it, since the reading itself rejects the hierarchy's legitimacy; victims are the same lower-caste and women's groups, but the reading's own diagnosis is that the hierarchy has no valid textual warrant at all). This hereditary_monopoly_reading is authored with the highest ε (0.65) among the three because it is the reading under which the standing hierarchical arrangement is treated as fully warranted and actively defended. All three share victim-group naming for cross-reading comparability but are NOT averaged or reconciled — each is a separate ε-invariant constraint per the decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
