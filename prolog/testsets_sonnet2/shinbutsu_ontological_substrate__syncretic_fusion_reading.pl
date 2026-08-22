% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku as Ontological Fusion of Kami and Buddhas
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the syncretic-fusion reading of the honji-suijaku
 *   kernel: the claim that kami and buddhas are ontologically identical, with
 *   kami being local manifestations (suijaku) of buddhas as their original
 *   ground (honji). Under this reading, the doctrine is metaphysically true,
 *   not merely an institutional accommodation between two coexisting
 *   religious systems. This is deliberately distinct from a domain-partition
 *   reading (kami and buddhas govern separate spheres, coexistence is
 *   functional) and from an incoherent-bundle reading (no unified doctrine
 *   exists at all, only accumulated institutional drift). Each reading is
 *   authored as its own constraint with its own epsilon; this file does not
 *   average across them or describe the contest internally.
 *
 * KEY AGENTS:
 *   - shingon_tendai_temple_complexes: agenda_setter/beneficiary (institutional/arbitrage) — administers the doctrinal and economic apparatus
 *   - independent_kami_cult_lineages: payer (moderate/trapped) — subordinated within the combinatory hierarchy
 *   - local_shrine_priests_excluded_from_combinatory_authority: payer (powerless/trapped) — bears the cost of doctrinal ranking without recourse
 *   - later_kokugaku_revivalists: excluded (organized/constrained) — the dissenting reading with no seat in this period's institutional order
 *   - shogunal_and_imperial_court_authorities: observer/agenda_setter (institutional/analytical) — grants recognition without fixed theological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku as Ontological Fusion of Kami and Buddhas").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'be784e73-8fc1-4e06-962f-c1fa72864cfa').
narrative_ontology:cs_kernel_codification('be784e73-8fc1-4e06-962f-c1fa72864cfa', distributed).
narrative_ontology:cs_authority_grounding('be784e73-8fc1-4e06-962f-c1fa72864cfa', lineage).
narrative_ontology:cs_interpretation_layer_present('be784e73-8fc1-4e06-962f-c1fa72864cfa').
narrative_ontology:cs_reading_relation('be784e73-8fc1-4e06-962f-c1fa72864cfa', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('be784e73-8fc1-4e06-962f-c1fa72864cfa', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('be784e73-8fc1-4e06-962f-c1fa72864cfa', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('be784e73-8fc1-4e06-962f-c1fa72864cfa', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('be784e73-8fc1-4e06-962f-c1fa72864cfa', foundational, honji_suijaku_as_metaphysical_manifestation_not_administrative_accord).
narrative_ontology:cs_axiom_status(honji_suijaku_as_metaphysical_manifestation_not_administrative_accord, overridden).
narrative_ontology:cs_axiom_grounding('be784e73-8fc1-4e06-962f-c1fa72864cfa', honji_suijaku_as_metaphysical_manifestation_not_administrative_accord, theological).
narrative_ontology:cs_reference_frame('be784e73-8fc1-4e06-962f-c1fa72864cfa', nara_heian_combinatory_orthodoxy).
narrative_ontology:cs_drift_state('be784e73-8fc1-4e06-962f-c1fa72864cfa', meiji_shinbutsu_bunri_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('be784e73-8fc1-4e06-962f-c1fa72864cfa', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, combinatory_shrine_temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, court_sanctioned_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_kami_cult_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, local_shrine_priests_excluded_from_combinatory_authority).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, later_kokugaku_revivalists).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_metaphysical_identity_thesis).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhas_as_original_ground_of_kami).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrinal apparatus that identifies specific kami as local manifestations (suijaku) of specific buddhas or bodhisattvas (honji). Controls the combinatory shrine-temple complexes (jingu-ji), ritual calendars, and the scholastic lineages that produce authoritative honji-suijaku pairings. Collects tithes, land grants, and pilgrimage revenue that flow through the fused institutional structure; the ontological claim of unity is inseparable from the administrative apparatus that profits from it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_temple_complexes, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_temple_complexes, beneficiary).

% Operate as fused sites where Buddhist ritual specialists and shrine priests share authority, land, and revenue under the honji-suijaku framework. Their continued existence depends on the ontological claim remaining unquestioned; a partition reading would force them to divide assets and jurisdiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, combinatory_shrine_temple_institutions, beneficiary,
    institutional, generational, constrained, regional).

% Scholar-priests who produce and certify honji-suijaku correspondences receive court patronage, teaching income, and interpretive authority. They can move between doctrinal schools if one loses favor, giving them more exit than the institutions whose assets are fixed to a given fusion claim.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, court_sanctioned_ritual_specialists, beneficiary,
    powerful, biographical, arbitrage, national).

% Local kami cults with pre-Buddhist lineages and distinct ritual practices are absorbed into the combinatory framework or delegitimized as unassimilated 'lesser' kami. Their local authority is subordinated to the honji-suijaku hierarchy that ranks kami by which buddha they manifest; cults that resist assimilation lose court recognition and patronage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_kami_cult_lineages, payer,
    moderate, generational, trapped, local).

% Hereditary shrine priests whose kami were not selected for prestigious honji-suijaku pairings lose access to the land grants and patronage flowing to jingu-ji complexes. They cannot contest the ontological claim without appearing to reject orthodox Buddhist cosmology, which forecloses political recourse.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, local_shrine_priests_excluded_from_combinatory_authority, payer,
    powerless, biographical, trapped, local).

% Edo-period nativist scholars who would argue kami require no buddha to ground their reality are structurally absent from the honji-suijaku institutional order this reading describes; their eventual success in the Meiji shinbutsu bunri separation shows the fusion claim was contestable, but within the period this constraint governs, they have no seat at the combinatory table.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, later_kokugaku_revivalists, excluded,
    organized, civilizational, constrained, national).

% Grant charters, land, and legal recognition to combinatory institutions, adjudicating disputes between rival honji-suijaku schemes without themselves holding a fixed theological position; their patronage decisions determine which fusion claims gain state backing and which wither.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shogunal_and_imperial_court_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shogunal_and_imperial_court_authorities, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_temple_complexes).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single cosmological grammar allowing Buddhist and kami-worship communities, ritual calendars, and sacred sites to operate within one institutional and doctrinal order rather than as two competing religious systems, reducing conflict between imported and indigenous cultic traditions.
% TRANSFER_FUNCTION: Moves ritual authority, land revenue, pilgrimage income, and court patronage from independent and lower-ranked kami lineages toward the temple complexes and ritual specialists who administer the honji-suijaku correspondences and the combinatory shrine-temple institutions built on them.
% ABSENT_VOICES: Later kokugaku revivalists who would deny that kami require a buddha as their 'original ground' are not present as a organized voice within the period this reading describes; excluded local shrine priests object informally but lack the doctrinal vocabulary or court standing to contest an ontological claim framed as metaphysical rather than political.
% DISAPPEARANCE_RATIONALE: If the ontological fusion claim were rejected, jingu-ji complexes would face the same asset and jurisdiction disputes the Meiji shinbutsu bunri edicts actually produced: land redistributed, priesthoods separated, ritual calendars split, and the ranked hierarchy of kami-as-manifestations dissolved into either independent kami cults or a purely functional partition.
% FOUNDING_PROBLEM: Early Buddhist missions to Japan needed a way to explain why worship of indigenous kami persisted despite Buddhist cosmological claims to universal truth, and needed a mechanism to fold existing sacred sites and their political backing into the Buddhist institutional order rather than displacing them.
% FOUNDING_PROBLEM_CORROBORATION: Temple complex records and court charters attest the fusion as settled doctrine for centuries. Meiji-era shinbutsu bunri policy architects and kokugaku scholars, writing from outside the benefiting combinatory institutions, attested that the 'ontological' unity was a doctrinal overlay serving temple economic interests and administrative convenience, not an inherited metaphysical necessity — their success in enforcing separation in 1868 is itself evidence the fusion was not treated as immovable by everyone.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58, moderate-high but not extreme: the coordination function (a shared cosmological grammar preventing sectarian conflict) is genuine and substantial, but land, patronage, and ritual authority visibly flow toward temple complexes and ranked honji-suijaku pairings at the expense of unassimilated kami lineages and lower-ranked shrine priests. Suppression rises from 0.40 to 0.62 across the interval as the combinatory institutions harden from informal syncretism into codified doctrine backed by court charter and enforced ranking — later periods make dissent from the ontological claim look like heterodoxy rather than a live alternative. Theater ratio is moderate (0.40) and rising: as the coordination function matures, an increasing share of ritual and doctrinal activity serves to perform and re-certify the fusion claim rather than to solve any live cosmological tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Temple complexes and court-sanctioned ritual specialists sit near the beneficiary end: they administer the fusion doctrine, collect its rents, and can move between doctrinal schools if a particular honji-suijaku scheme falls from favor. Independent kami lineages and excluded local priests sit near the target end: trapped by locality and hereditary office, they cannot exit the framework without losing court recognition entirely, and their subordination is enacted through the same ontological claim that the beneficiaries administer. Kokugaku revivalists are excluded rather than coordinated or extracted from within this period — their absence from the institutional table is itself the structural fact the domain-partition and incoherent-bundle siblings would treat differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an imported universal cosmology with entrenched local cult practice and political backing — was substantially live in the Nara-Heian period and became progressively less live as Buddhist institutional dominance consolidated; by the medieval period the arrangement primarily protects accumulated temple assets and ranked authority rather than solving an active syncretism problem. This is a contested founding-problem status (not resolved dead) because temple institutions continued to assert the metaphysical necessity of fusion even as its coordination function atrophied into an inertial, then actively defended, arrangement — precisely the drift the Meiji separation edicts later targeted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_claim_vs_administrative_convenience,
    'Was the honji-suijaku fusion doctrine a genuine metaphysical commitment held by its promulgators, or a theological gloss constructed to legitimate an administrative merger of temple and shrine assets?',
    'Comparative analysis of doctrinal writings across schools (Sanno-ichijitsu, Ryobu Shinto) for internal metaphysical consistency versus correlation between honji-suijaku pairing prestige and land-grant value; convergence of doctrine with asset flows would support the administrative-convenience reading.',
    'If administrative convenience, this reading''s claimed ontological unity is itself a legitimating superstructure over extraction, strengthening the tangled_rope classification and weakening the metaphysical-truth claim''s independence from institutional interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_claim_vs_administrative_convenience, conceptual, 'Whether ontological fusion is a sincere metaphysical claim or a legitimating gloss on asset consolidation.').

omega_variable(
    kernel_committer_structure,
    'Among the three declared readings of the shinbutsu_ontological_substrate kernel (syncretic_fusion, domain_partition, incoherent_bundle), which reading did specific historical actors actually hold, and did any single actor hold more than one reading across their lifetime or across ritual versus doctrinal contexts?',
    'Textual analysis distinguishing ritual-manual language (which may presuppose functional partition) from scholastic-doctrinal treatises (which may assert full ontological identity) within the corpus of a single school or even a single author, to test whether the readings were held as exclusive commitments or context-dependent registers.',
    'If individual actors moved fluidly between readings depending on context, the kernel itself may be better modeled as under-determined (supporting incoherent_bundle) rather than as three cleanly separable commitments; if actors held one reading consistently and treated the others as heterodox, the forecloses/coexists_with structure in cs_structure.reading_relations should be revisited.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Whether historical actors held the three kernel readings as exclusive, coexisting, or fluid context-dependent commitments.').

omega_variable(
    meiji_separation_as_repudiation_evidence,
    'Does the Meiji government''s successful enforcement of shinbutsu bunri (forced separation) in 1868 constitute decisive evidence that the syncretic_fusion_reading''s ontological claim was never as metaphysically settled as its own institutions asserted, or was the separation a political imposition that overrode a genuinely-held metaphysical consensus by force?',
    'Examine resistance patterns to the 1868 edicts: widespread, doctrinally-grounded resistance from temple institutions would support the sincerity of the fusion claim even under this reading; rapid, largely administrative compliance would support the reading that fusion was primarily institutional cover.',
    'Bears directly on whether this reading''s claimed_type of tangled_rope (genuine coordination plus extraction) should shift toward snare (extraction with a thin coordination cover) if compliance was rapid and non-doctrinal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_repudiation_evidence, empirical, 'Whether the ease of forced separation in 1868 undermines the sincerity of the ontological fusion claim within this reading''s own period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 80, 0.59).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_substrate kernel, each authored as a separate story per the ε-invariance principle. syncretic_fusion_reading (this file) claims full ontological identity between kami and buddhas and is authored with the highest institutional entanglement and highest resistance to separation of the three. domain_partition_reading claims functional coexistence across separate domains with lower entanglement. incoherent_bundle_reading denies any unified kernel exists at all, treating the entire arrangement as accumulated institutional drift. All three link to each other via affects_constraints because they compete for the same historical evidentiary record and institutional space; adjudicating between them (e.g. via the omega on committer structure) has direct implications for all three siblings' classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
