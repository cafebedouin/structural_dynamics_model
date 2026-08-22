% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Institutionally-Maintained Incoherent Bundle (pre-Meiji)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Shinbutsu-shugo, the pre-Meiji coexistence of kami worship and Buddhist
 *   practice in Japan, is read here as an incoherent bundle: a set of
 *   overlapping institutional arrangements (combined shrine-temple complexes,
 *   honji suijaku theorizing, shared clergy, shared festival calendars,
 *   shogunate temple registration) that never rested on a settled answer to
 *   the ontological question of what kami and buddhas actually are relative
 *   to each other. The bundle persisted for centuries not because it was
 *   coherent but because every major institutional actor — shrine-temple
 *   administrators, ascetic lineages, and the shogunal registration system —
 *   benefited from the question staying open, while lay practitioners bore
 *   the confusion costs and kokugaku critics found no single target to
 *   attack. When the Meiji state needed a doctrinally clean, unified national
 *   Shinto to anchor imperial legitimacy, it discovered there was no clean
 *   boundary to draw and had to manufacture one by force (shinbutsu bunri),
 *   destroying jingu-ji complexes and reclassifying clergy nationwide. This
 *   story authors ε for the standing pre-Meiji bundled arrangement as this
 *   reading sees it — substantially extractive of interpretive clarity and
 *   ritual autonomy from lay practitioners, and load-bearing enough
 *   institutionally that its dissolution required a violent, decade-long
 *   state campaign, not a mere paperwork change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.61).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Institutionally-Maintained Incoherent Bundle (pre-Meiji)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '5a3b0b4c-48eb-4c00-bb9f-53dd76259632').
narrative_ontology:cs_kernel_codification('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', distributed).
narrative_ontology:cs_authority_grounding('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', practice).
narrative_ontology:cs_interpretation_layer_present('5a3b0b4c-48eb-4c00-bb9f-53dd76259632').
narrative_ontology:cs_reading_relation('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', foundational, no_stable_shared_ontology_ever_existed).
narrative_ontology:cs_axiom_status(no_stable_shared_ontology_ever_existed, holdable).
narrative_ontology:cs_axiom_grounding('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', no_stable_shared_ontology_ever_existed, empirically_contingent).
narrative_ontology:cs_axiom('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', foundational, ambiguity_is_a_maintained_institutional_resource_not_an_accident).
narrative_ontology:cs_axiom_status(ambiguity_is_a_maintained_institutional_resource_not_an_accident, holdable).
narrative_ontology:cs_axiom_grounding('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', ambiguity_is_a_maintained_institutional_resource_not_an_accident, empirically_contingent).
narrative_ontology:cs_axiom('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', secondary, meiji_separation_reveals_rather_than_manufactures_incoherence).
narrative_ontology:cs_axiom_status(meiji_separation_reveals_rather_than_manufactures_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', meiji_separation_reveals_rather_than_manufactures_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', unexamined_administrative_coexistence).
narrative_ontology:cs_drift_state('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', meiji_shinbutsu_bunri_decrees, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('5a3b0b4c-48eb-4c00-bb9f-53dd76259632', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complex_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_ascetic_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_temple_registration_system).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_nativist_scholars).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_shinto_architects).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional_ambiguity_can_outlast_doctrinal_incoherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run combined shrine-temple compounds (jingu-ji) where a single institutional apparatus collects offerings, registers parishioners under the danka system, and performs both kami rites and Buddhist rites without ever being required to state whether these address the same reality. They administer ambiguity as a resource: any doctrinal challenge can be deflected by pointing to the other half of the bundle.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complex_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complex_administrators, beneficiary).

% Mountain ascetic orders (Shugendo) draw authority precisely from moving fluidly between kami-worship and esoteric Buddhist practice at sites like Kumano and Omine. Their ritual and economic standing depends on the boundary between kami and buddha staying unresolved; a forced ontological answer in either direction would dissolve their distinctive practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugendo_ascetic_lineages, beneficiary,
    organized, generational, mobile, regional).

% The shogunate uses temple registration (terauke) as a census and surveillance mechanism, requiring every household to affiliate with a Buddhist temple regardless of local kami devotion. It benefits from the bundle staying unexamined because the registration apparatus rides on Buddhist institutional infrastructure while tolerating kami practice underneath it, avoiding a costly reckoning over which loyalty is primary.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_temple_registration_system, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tokugawa_temple_registration_system, agenda_setter).

% Ordinary villagers participate in both kami festivals and Buddhist funeral rites without any authority ever telling them how the two fit together, or whether they are being asked to hold contradictory metaphysical commitments. They bear the cost of unresolved incoherence as confusion, redundant ritual obligation, and dependence on local clergy to mediate a system no one can fully explain.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Edo-period nativist scholars (Motoori Norinaga's successors) argue that Buddhist accretion has corrupted an original pure kami tradition. They are structurally disadvantaged by the bundle because dismantling it requires defeating an entrenched institutional apparatus that has no single doctrinal target to attack — the incoherence itself absorbs their critique without ever conceding the point.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_nativist_scholars, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_nativist_scholars, excluded).

% After 1868, state officials attempting to construct a coherent national Shinto need a clean kami ontology to ground imperial legitimacy. They inherit centuries of unexamined bundling and must pay the cost of forcibly separating (shinbutsu bunri) what was never actually unified — destroying jingu-ji complexes, defrocking syncretic clergy, and confiscating combined properties, a costly and violent disentanglement of something with no coherent joints to cut along.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_shinto_architects, payer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_shinto_architects, agenda_setter).

% Later historians and religious studies scholars examine the institutional record and argue the bundle was never a stable ontological synthesis at all, but an administrative and ritual arrangement that different actors invoked for different purposes, whose apparent coherence was maintained by nobody ever being forced to answer the categorical question directly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundle allows a single ritual-administrative apparatus to serve overlapping communal needs (agricultural/purity rites via kami, funerary/salvific rites via Buddhism, census/loyalty registration via temple affiliation) without requiring any actor to resolve whether kami and buddhas are the same beings, different beings, or incommensurable categories — coordination is achieved by never asking the question.
% TRANSFER_FUNCTION: Moves ritual fees, land grants, and forced temple registration payments from lay households to combined shrine-temple institutions and their patron authorities (shogunate, domain lords), while moving interpretive labor and confusion costs onto lay practitioners and onto any scholar or reformer who tries to render the system doctrinally consistent.
% ABSENT_VOICES: Lay practitioners are never consulted on the reconciliation question at all — the ambiguity is maintained by clerical and administrative elites who have every incentive to prevent a definitive answer from emerging, since a definitive answer in either direction would eliminate one revenue stream or the other.
% DISAPPEARANCE_RATIONALE: The Meiji shinbutsu bunri decrees (1868 onward) show exactly what happens when the bundle is forcibly dissolved: combined shrine-temple complexes are physically dismantled, tens of thousands of Buddhist images and implements are removed from shrines, clergy are reclassified or defrocked, and property, ritual calendars, and local religious authority are all reorganized from the ground up. The magnitude of that rearrangement is itself evidence that a real, load-bearing institutional structure — not merely a philosophical label — depended on the ambiguity persisting.
% FOUNDING_PROBLEM: Early esoteric Buddhist institutions needed to establish legitimacy and ritual authority over territory already saturated with kami cults; rather than displacing or theologically defeating kami worship, Buddhist institutions absorbed it administratively (honji suijaku theorizing came later, as post-hoc justification for an arrangement that began as practical coexistence and institutional land-grabbing).
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state officials and kokugaku scholars — both outside the shrine-temple administrator beneficiary class — attest that the arrangement had long since stopped solving any live problem and instead functioned as accumulated institutional inertia defended by clergy with property and status at stake; modern religious studies scholarship (Kuroda Toshio's kenmitsu taisei framework and successors) independently corroborates that the 'coherent synthesis' story was substantially a retrospective gloss on what was originally an unprincipled administrative accretion.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61 at the end of the pre-Meiji interval) reflects the cumulative cost the ambiguity imposes on lay practitioners (forced dual ritual obligation, no doctrinal clarity) and on reform-minded scholars (no coherent target to argue against) rather than a formal declared tax; the theater ratio is high and rising (0.72 by late Tokugawa) because an increasing share of institutional activity — honji suijaku commentary, ritual harmonization texts — functions to perform coherence for external legitimacy rather than to resolve or even genuinely address the underlying categorical incoherence. Suppression (0.58) is comparatively lower than extraction because the bundle did not require heavy-handed coercion to persist during most of its run; it persisted through diffusion of responsibility and the absence of any single actor with standing to demand an answer, not through active enforcement against dissent — until the very end, when the shogunate's need to prevent kokugaku-driven destabilization required some suppression of separatist agitation. At time_point 100 (the Meiji rupture), theater_ratio and base_extractiveness collapse sharply because the bundle itself is being dismantled — the numbers describe the pre-Meiji arrangement's own trajectory ending in its forcible termination, not a smooth continuation; suppression_requirement spikes at 100 because dismantling the bundle itself required the most concentrated coercive campaign in the whole sequence (shrine-temple property seizure, clergy reclassification, image destruction).
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrators and shugendo lineages sit near the full-beneficiary end: they set the terms of the ambiguous arrangement and their institutional survival depends on it remaining unresolved, so directionality derives low. The Tokugawa registration system is also structurally a beneficiary despite being a state apparatus, because it uses the bundle instrumentally for surveillance without needing to resolve it theologically. Lay practitioners are trapped targets — no meaningful exit from a religious infrastructure that is also civil registration — and correspondingly derive high directionality toward extraction. Kokugaku scholars and Meiji state architects are payers of a different kind: not economically extracted from in the ordinary sense, but structurally disadvantaged by having to fight or dismantle an opponent with no single coherent doctrinal position to refute or negotiate with — their cost is the disproportionate difficulty and violence of undoing something that resists being cleanly undone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early Buddhist institutions needing practical accommodation with entrenched kami cults) was solved centuries before the Meiji restoration — by the Edo period the coexistence was thoroughly naturalized and no longer solved any live problem, it simply persisted as the path of least institutional resistance. Declaring founding_problem_status as dead and cross-checking against the world_rearranges disappearance verdict surfaces exactly the capture/zombie pattern the R5 interview is designed to catch: an arrangement whose original justification had long expired persisted anyway because dismantling it was costlier to any single actor than tolerating it, until an external shock (Meiji state-building imperatives) made the cost of continued toleration exceed the cost of forced separation for the newly empowered state actor. This is why the classification computes closer to piton than snare from the administrator seat's own vantage: no single beneficiary was extracting a concentrated, identifiable rent large enough to explain the persistence on its own — the persistence is better explained by diffuse institutional inertia across several beneficiary groups, each individually invested in a fraction of the ambiguity, none of them the primary architect of the whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_vs_synthesis_ambiguity,
    'Was shinbutsu-shugo a genuinely (if implicitly) coherent syncretic ontology that Meiji nationalism destroyed for political reasons, or was it always an incoherent administrative bundle whose incoherence Meiji pressure merely exposed?',
    'Comparative textual analysis of pre-Edo honji suijaku doctrinal writings for internal ontological consistency across different lineages and regions; if different jingu-ji complexes held mutually incompatible accounts of the kami-buddha relationship without any adjudicating mechanism, that supports the incoherent-bundle reading over the syncretic-fusion reading.',
    'If the bundle was genuinely coherent, this story''s claimed_type and extraction attribution overstate the case and the constraint is better read as the syncretic_fusion_reading''s rope/tangled_rope structure; if genuinely incoherent, the piton/extraction framing here holds and the syncretic doctrine is better read as retrospective legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_vs_synthesis_ambiguity, conceptual, 'Whether the bundle had a real (if implicit) ontology or was purely administratively sustained ambiguity — the central committer-frame question distinguishing this reading from syncretic_fusion_reading.').

omega_variable(
    domain_partition_vs_true_incoherence,
    'Did kami and buddhas actually govern cleanly separated existential domains in practice (life/purity vs. death/salvation), making the arrangement a stable partition rather than an incoherent bundle, with Meiji bunri simply formalizing a pre-existing division?',
    'Historical-ethnographic survey of ritual calendars and clergy jurisdiction at a representative sample of jingu-ji complexes: consistent, non-overlapping domain assignment across sites would support domain_partition_reading; inconsistent or contested assignment (same rite claimed by both kami and Buddhist clergy at different sites or periods) supports incoherent_bundle_reading.',
    'If domain partition was genuinely stable and consistent, the Meiji violence reflects imposed novelty rather than revealed incoherence, and this reading''s extraction attribution (confusion cost borne by lay practitioners) would need substantial revision downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_true_incoherence, empirical, 'Whether apparent domain separation was a real stable structure this reading denies, or itself another instance of the ambiguity this reading identifies — the second committer-frame axis distinguishing this reading from domain_partition_reading.').

omega_variable(
    beneficiary_concentration_ambiguity,
    'Is the extraction from lay practitioners diffuse enough across multiple beneficiary institutions to genuinely support a piton classification, or does deeper archival work reveal a more concentrated beneficiary (e.g., specific dominant temple lineages capturing disproportionate rents) that would push this toward snare or tangled_rope instead?',
    'Land-grant and koku-daka (rice-yield assessment) records for major jingu-ji complexes compared against smaller rural shrine-temple pairings, to determine whether extraction was genuinely dispersed or concentrated in a small number of dominant institutions.',
    'Concentrated capture would shift the classification toward snare/tangled_rope with a named primary beneficiary; genuinely dispersed capture supports the piton reading authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_concentration_ambiguity, empirical, 'Whether the diffuse-beneficiary piton framing survives closer archival scrutiny of actual rent concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(shin_tr_t20, projected).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 60, 0.63).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 80, 0.68).
narrative_ontology:measurement(shin_tr_t95, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 95, 0.72).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(shin_be_t20, projected).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(shin_be_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement(shin_be_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(shin_be_t95, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 95, 0.61).
narrative_ontology:measurement(shin_be_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(shin_su_t20, projected).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(shin_su_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(shin_su_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(shin_su_t95, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 95, 0.58).
narrative_ontology:measurement(shin_su_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_shinbutsu_bunri_separation_decrees).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, state_shinto_construction).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the shinbutsu_coexistence_commitment kernel (incoherent_bundle_reading, alongside syncretic_fusion_reading and domain_partition_reading), decomposed per the epsilon-invariance principle because the three readings assign structurally different ontological status and different beneficiary/victim sets to the same historical arrangement. It also links forward to meiji_shinbutsu_bunri_separation_decrees and state_shinto_construction as the downstream constraints this reading's collapse feeds into.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
