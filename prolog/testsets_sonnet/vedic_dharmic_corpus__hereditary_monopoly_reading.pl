% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Hereditary Brahmin Monopoly on Ritual and Interpretive Authority
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This story instantiates one reading among three of the contested
 *   vedic_dharmic_corpus kernel: the hereditary monopoly reading, which holds
 *   that ritual and interpretive authority derive from birth into Brahmin
 *   lineage and that varna hierarchy is divinely ordained and textually
 *   prescribed (drawing on passages such as the Purusha Sukta and portions of
 *   the Dharmashastra corpus, particularly Manusmriti). This reading is
 *   generated as a clean, self-contained constraint with its own stable
 *   extraction profile — it does not describe or average over the sibling
 *   bhakti_devotional_reading (which holds that sincere devotion bypasses
 *   caste requirement) or the reformist_egalitarian_reading (which holds that
 *   constitutional equality principles must govern textual interpretation and
 *   that caste hierarchy is historical accretion). Those are separate
 *   constraints in separate files, linked here only via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_lineages: agenda_setter/beneficiary (institutional/arbitrage) — administer ritual gatekeeping and collect its rents
 *   - temple_administrative_authorities: beneficiary/agenda_setter (institutional/constrained) — convert ritual authority into endowment and access control
 *   - lower_caste_communities: payer (powerless/trapped) — excluded from ritual and interpretive participation by birth
 *   - dalit_communities: payer (powerless/trapped) — excluded most severely, positioned outside the hierarchy entirely
 *   - women_across_varnas: payer (powerless/trapped) — excluded from independent ritual authority regardless of birth varna
 *   - bhakti_practitioner_communities: excluded (organized/constrained) — contest the reading from an adjacent devotional frame
 *   - reformist_legal_and_constitutional_bodies: excluded (institutional/analytical) — contest the reading's practical consequences from the state's legal plane
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
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Monopoly on Ritual and Interpretive Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, 'd202db98-77c6-406e-9e78-23b29efb52fa').
narrative_ontology:cs_kernel_codification('d202db98-77c6-406e-9e78-23b29efb52fa', fixed_text).
narrative_ontology:cs_authority_grounding('d202db98-77c6-406e-9e78-23b29efb52fa', lineage).
narrative_ontology:cs_interpretation_layer_present('d202db98-77c6-406e-9e78-23b29efb52fa').
narrative_ontology:cs_reading_relation('d202db98-77c6-406e-9e78-23b29efb52fa', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d202db98-77c6-406e-9e78-23b29efb52fa', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('d202db98-77c6-406e-9e78-23b29efb52fa', foundational, birth_lineage_grounds_ritual_authority).
narrative_ontology:cs_axiom_status(birth_lineage_grounds_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('d202db98-77c6-406e-9e78-23b29efb52fa', birth_lineage_grounds_ritual_authority, theological).
narrative_ontology:cs_axiom('d202db98-77c6-406e-9e78-23b29efb52fa', foundational, varna_hierarchy_divinely_prescribed).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_prescribed, holdable).
narrative_ontology:cs_axiom_grounding('d202db98-77c6-406e-9e78-23b29efb52fa', varna_hierarchy_divinely_prescribed, theological).
narrative_ontology:cs_reference_frame('d202db98-77c6-406e-9e78-23b29efb52fa', vedic_ritual_transmission_order).
narrative_ontology:cs_drift_state('d202db98-77c6-406e-9e78-23b29efb52fa', post_constitutional_egalitarian_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d202db98-77c6-406e-9e78-23b29efb52fa', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_authorities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, varna_divine_ordination_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, textual_prescriptive_inerrancy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer ritual performance, textual transmission, and interpretive gatekeeping by hereditary right. Control access to sacred rites (upanayana, temple entry, scriptural instruction) and collect ritual fees, land grants, and social deference as a function of birth status rather than demonstrated competence or devotion. Can move between regions and institutions while retaining status; the constraint's persistence is their institutional project.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_lineages, beneficiary).

% Manage temple endowments, ritual calendars, and entry restrictions, drawing legitimacy and revenue from enforcing the hereditary reading. Historically excluded lower-caste worshippers from inner sanctums and controlled access to darshan and priestly office, converting religious authority into material and political capital.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_authorities, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, temple_administrative_authorities, agenda_setter).

% Denied temple entry, priestly office, and independent scriptural study on grounds of birth status regardless of learning, devotion, or conduct. Bear social exclusion, economic subordination through caste-linked occupation, and reduced access to legal and social recognition. Historically had no exit from caste designation short of conversion out of the tradition entirely, which itself carried severe social cost.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_communities, payer,
    powerless, generational, trapped, regional).

% Positioned outside the varna hierarchy entirely by this reading, and subjected to the most severe forms of ritual and social exclusion (untouchability, denial of temple access, segregated water and living arrangements). The hereditary monopoly's textual prescriptions have historically been cited as divine sanction for these exclusions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, dalit_communities, payer,
    powerless, generational, trapped, regional).

% Excluded from independent ritual and interpretive authority regardless of birth varna; scriptural study, mantra recitation rights, and priestly function are reserved to men within the eligible lineages under this reading. Access to religious standing is mediated through male relatives, and exit from the constraint means exit from full religious participation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas, payer,
    powerless, generational, trapped, regional).

% Hold that sincere devotion, not birth, grants spiritual authority, and have historically built parallel devotional institutions (bhakti movements, saint-poet traditions) outside Brahmin control. Their claim is structurally present in the tradition but is not part of the hereditary monopoly reading's own account of legitimate authority; they contest this reading from outside its interpretive frame rather than within it.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_practitioner_communities, excluded,
    organized, generational, constrained, regional).

% Modern constitutional and legislative bodies (anti-untouchability law, temple-entry legislation, reservation policy) have intervened against the practical consequences of this reading, but the hereditary monopoly reading does not recognize their authority as competent to revise scriptural meaning; their critique operates on the state's legal plane, not within this reading's interpretive plane.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_legal_and_constitutional_bodies, excluded,
    institutional, generational, analytical, national).

% Study textual layers (Purusha Sukta, Manusmriti, Dharmashastra commentary traditions) and the historical development of varna practice, documenting both the antiquity of hierarchical claims and their contested, regionally variable application. Their scholarship is used by all three kernel readings to support divergent conclusions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, comparative_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible system for preserving ritual technique, Sanskrit textual corpora, and liturgical performance across generations through a designated hereditary custodial class, reducing the coordination cost of maintaining complex oral and ritual traditions absent centralized institutional infrastructure.
% TRANSFER_FUNCTION: Moves ritual fees, land endowments, social deference, temple access, and interpretive authority from lower-caste communities, Dalits, and women toward Brahmin lineages and the temple administrations they control, justified as the correct execution of divinely prescribed order rather than as a transfer.
% ABSENT_VOICES: Bhakti practitioners who claim devotion supersedes birth, and reformist/constitutional bodies who hold caste hierarchy is historical accretion rather than scriptural essence, are structurally present in the broader tradition but excluded from this reading's own account of who may legitimately interpret; their objections are voiced in adjacent institutions (devotional movements, courts, legislatures), not inside this reading's interpretive process.
% DISAPPEARANCE_RATIONALE: If hereditary ritual monopoly and its enforcement mechanisms (temple entry restriction, priestly office reservation, endowment control) disappeared overnight, temple administration, ritual fee structures, and priestly office would have to reorganize around competence- or devotion-based criteria; lower-caste and women's access to ritual participation would expand immediately; Brahmin lineages would lose a structural source of institutional revenue and status distinct from individual scholarly achievement.
% FOUNDING_PROBLEM: Preservation and faithful oral/ritual transmission of complex Vedic liturgical technique and Sanskrit textual corpora across many generations without writing or centralized institutions, requiring a dedicated custodial class trained from childhood.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin lineages and temple authorities attest the founding problem remains live, citing the continued technical difficulty of correct Vedic recitation and ritual performance. Comparative religious historians and reformist constitutional bodies, outside the beneficiary class, attest that textual transmission no longer requires hereditary exclusivity given widespread literacy, textual criticism, and open scholarly access to primary sources, and that the hereditary requirement now functions primarily to preserve status and revenue rather than to solve a live transmission problem.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.65 reflecting a substantial, structurally embedded transfer of ritual fees, land endowment, temple access, and interpretive standing from excluded groups to hereditary Brahmin lineages and temple administrations — persistent across the measured interval (0.55 to 0.65) as the ritual economy matured and consolidated, with a modest dip around t=80 reflecting periods of reformist and devotional pressure before renewed institutional consolidation. Suppression is authored higher (0.78) than extraction because the constraint's persistence has historically depended on active exclusion mechanisms (temple entry bans, denial of scriptural access, untouchability practice) rather than voluntary participant preference — suppression here is the raw structural property, not scaled by scope, per framework rule. Theater ratio rises over the interval (0.20 to 0.42) as some ritual gatekeeping increasingly functions as status performance and revenue mechanism even where the original transmission function (technical fidelity of oral recitation) has become less load-bearing given widespread literacy and textual publication.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Brahmin lineages, temple authorities), the arrangement is experienced as sacred coordination — correct preservation of a divinely revealed order. From the payer seats (lower castes, Dalits, women), the identical structure is experienced as enforced exclusion with no meaningful exit. The engine computes these divergent per-seat classifications from the same structural data; this story does not adjudicate which experience is 'true' — it authors the structural facts (who sets terms, who bears cost, what exit looks like) that produce the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin lineages and temple authorities sit at the beneficiary end of directionality: they set the interpretive terms, administer the enforcement machinery, and collect the transfer (ritual fees, land grants, deference, exclusive office) — their exit options are effectively arbitrage, since they can move between institutional settings while retaining hereditary status. Lower-caste communities, Dalits, and women sit at the full-target end: the constraint's declared victim groups bear the costs of exclusion with historically trapped exit options (conversion out of the tradition carried severe social cost and did not always escape caste-linked social treatment). This is a straightforward beneficiary/victim derivation with no override needed — the structural asymmetry is stark and well-documented.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration fields surface a genuine mismatch: the reading's own beneficiaries (Brahmin lineages) attest the founding transmission problem remains live, while outside corroborators (historians, reformist institutions) attest the problem is largely solved by literacy and textual scholarship, and that the hereditary requirement now functions mainly to preserve status and revenue. This status=contested + verdict=world_rearranges pairing is exactly the signal the mismatch-consumer is built to catch — the founding narrative should not be taken as self-validating, and the persistence of hereditary exclusivity long after its stated transmission rationale weakened is read here as a live capture/zombie-function candidate rather than settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordination_vs_historical_construction,
    'Is the varna hierarchy this reading treats as divinely ordained and textually prescribed a genuine feature of the earliest textual layer, or a later social construction retrojected onto and consolidated through selective textual emphasis?',
    'Comparative philological analysis of textual strata (early Vedic corpus versus later Dharmashastra elaboration), combined with archaeological and epigraphic evidence of historical varna/jati practice variation across regions and periods.',
    'If varna hierarchy is a later social construction rather than an original textual essence, this reading''s central legitimating claim (divine ordination) is substantially weakened, supporting a stronger reading of the constraint as constructed extraction wearing a natural-law/theological cover story rather than a genuine, stable theological commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordination_vs_historical_construction, conceptual, 'Whether varna hierarchy is original textual essence or historical accretion — the core dispute this reading has with reformist_egalitarian_reading.').

omega_variable(
    coordination_transmission_residual_value,
    'How much of the original oral-transmission coordination function (technical fidelity of Vedic recitation, ritual sequence preservation) remains genuinely load-bearing today, versus having been substantially replaced by textual publication, audio recording, and academic Sanskrit scholarship?',
    'Comparative assessment of ritual/recitation fidelity in hereditary-lineage-trained practitioners versus non-hereditary scholars trained through modern academic or open pedagogical routes.',
    'If residual transmission value is low, the tangled_rope classification''s coordination component is weak relative to its extraction component, pushing the constraint''s actual operation closer to snare; if residual value remains substantial, the coordination function partially justifies continued hereditary specialization independent of the hierarchy''s exclusionary features.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_transmission_residual_value, empirical, 'Whether the original coordination rationale still carries real weight or has been substantially superseded.').

omega_variable(
    reading_selection_provenance,
    'Two coherent framings of the underlying kernel are available: reading the varna prescriptions as the kernel''s operative content (this reading), versus reading a layered/composite textual tradition where hierarchical passages coexist with devotional and egalitarian strands of comparable antiquity (undermining any single reading''s claim to be THE textual essence). This story adopts the first framing per its assigned reading_id.',
    'Textual-critical consensus on which strands of the corpus are earliest/most authoritative, and whether the tradition itself has ever had a single unified operative reading versus always having been genuinely plural.',
    'If the composite/plural framing is correct, no single reading (including this one) can claim exclusive textual essence, and the three-reading kernel decomposition itself should be understood as capturing a genuinely irreducible plurality rather than three competing claims to a single truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_provenance, conceptual, 'CS-framing under-determination: whether this reading captures the kernel''s dominant historical operative content or merely one strand among originally co-equal strands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(vedi_su_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.08).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the vedic_dharmic_corpus kernel, decomposed per the ε-invariance principle rather than represented as a single constraint with a measurement parameter. hereditary_monopoly_reading (this file) carries high extraction (ε~0.65) with a clear beneficiary (Brahmin priestly class) and victim set (lower castes, Dalits, women), enforced through temple control and ritual economy. bhakti_devotional_reading is expected to show substantially lower extraction, since it treats devotional sincerity rather than birth as sufficient for spiritual authority, structurally bypassing the hereditary gate. reformist_egalitarian_reading is expected to show a coordination function oriented toward constitutional equality rather than ritual preservation, with extraction concentrated instead in whatever resistance the reform imposes on incumbent beneficiaries. All three readings are linked bidirectionally via affects_constraints and via cs_structure.reading_relations; none averages or represents the others' metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
