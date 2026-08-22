% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle: Enforced Syncretism Without Kernel
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This story reads the shinbutsu-shugo (kami-buddha combinatory)
 *   arrangement in premodern Japan not as a genuine metaphysical unity (the
 *   syncretic_fusion_reading) nor as a stable functional division of
 *   religious labor (the domain_partition_reading), but as an incoherent
 *   institutional bundle: accumulated administrative drift under state
 *   enforcement (particularly intensifying under Edo-period danka
 *   registration and jingu-ji institutionalization) that was never required
 *   to cohere doctrinally and was defended only for as long as it served
 *   state and institutional revenue interests. On this reading, the
 *   appearance of a settled honji suijaku doctrine ('buddhas as original
 *   ground, kami as local traces') is retrospective institutional packaging
 *   over what was, empirically, wide regional and sectarian variation with no
 *   single content. The rapid Meiji-era dismantling (shinbutsu bunri, 1868)
 *   is read as decisive evidence: an arrangement defended as sacred truth
 *   does not usually collapse within a few state-administrative years once
 *   its enforcing patron withdraws.
 *
 * KEY AGENTS:
 *   - shogunate_religious_administration: primary agenda-setter, benefits from administrative legibility
 *   - temple_shrine_complexes_beneficiary_class: institutional beneficiary of dual revenue and land grants
 *   - danka_registration_authorities: enforcement mechanism binding population to the fused system
 *   - lay_practitioners: bear the cognitive/practical cost of unresolved contradictory frameworks
 *   - shrine_priests_subordinated: institutionally forced into a Buddhist-framed account of their own cosmology
 *   - village_ritual_specialists: local practice flattened into sanctioned vocabulary to avoid suppression
 *   - doctrinal_historians: analytical observers documenting the absence of uniform content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle: Enforced Syncretism Without Kernel").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'bdf042c7-2a92-42bb-a837-e2b2d163ec67').
narrative_ontology:cs_kernel_codification('bdf042c7-2a92-42bb-a837-e2b2d163ec67', distributed).
narrative_ontology:cs_authority_grounding('bdf042c7-2a92-42bb-a837-e2b2d163ec67', extraction).
narrative_ontology:cs_interpretation_layer_present('bdf042c7-2a92-42bb-a837-e2b2d163ec67').
narrative_ontology:cs_reading_relation('bdf042c7-2a92-42bb-a837-e2b2d163ec67', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('bdf042c7-2a92-42bb-a837-e2b2d163ec67', shinbutsu_ontological_substrate__domain_partition_reading, influences).
narrative_ontology:cs_axiom('bdf042c7-2a92-42bb-a837-e2b2d163ec67', foundational, no_unified_commitment_ever_existed).
narrative_ontology:cs_axiom_status(no_unified_commitment_ever_existed, holdable).
narrative_ontology:cs_axiom_grounding('bdf042c7-2a92-42bb-a837-e2b2d163ec67', no_unified_commitment_ever_existed, empirically_contingent).
narrative_ontology:cs_axiom('bdf042c7-2a92-42bb-a837-e2b2d163ec67', secondary, apparent_doctrine_is_retrospective_institutional_packaging).
narrative_ontology:cs_axiom_status(apparent_doctrine_is_retrospective_institutional_packaging, holdable).
narrative_ontology:cs_axiom_grounding('bdf042c7-2a92-42bb-a837-e2b2d163ec67', apparent_doctrine_is_retrospective_institutional_packaging, empirically_contingent).
narrative_ontology:cs_reference_frame('bdf042c7-2a92-42bb-a837-e2b2d163ec67', pre_combinatory_distinct_kami_and_buddhist_traditions).
narrative_ontology:cs_drift_state('bdf042c7-2a92-42bb-a837-e2b2d163ec67', edo_period_danka_consolidation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('bdf042c7-2a92-42bb-a837-e2b2d163ec67', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shogunate_religious_administration).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_complexes_beneficiary_class).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_registration_authorities).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests_subordinated).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the temple registration (danka) system and the honji suijaku institutional apparatus binding shrines to Buddhist temple oversight. Enforces combinatory shrine-temple complexes (jingu-ji) as a matter of state religious policy and population control, not because any settled doctrine requires it. Benefits from a population whose religious obligations are legible, taxable, and administratively fused into one reporting structure. Has no need for the fusion to be coherent — only for it to be stable and enforceable.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shogunate_religious_administration, agenda_setter,
    institutional, generational, analytical, national).

% Combinatory temple-shrine institutions (jingu-ji) collect revenue and land grants from the fused arrangement, administering both kami rites and Buddhist rites under one institutional roof. Their economic position depends on the fusion persisting as an administrative fact; they have no doctrinal stake in resolving whether kami are avatars, separate beings, or something else. Exit from the arrangement would mean losing dual revenue streams and institutional standing built on the very ambiguity being contested.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, temple_shrine_complexes_beneficiary_class, beneficiary,
    organized, generational, arbitrage, regional).

% Local temple registries administer mandatory household registration used for census, tax, and anti-Christian surveillance purposes. The registries require every household to be affiliated with a Buddhist temple regardless of actual kami-worship practice or belief. This produces the appearance of settled religious identity while resolving nothing about what householders actually hold to be true about kami and buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_registration_authorities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, danka_registration_authorities, agenda_setter).

% Ordinary households are required to register with a temple, participate in both kami festivals and Buddhist funerary rites, and accept whatever combinatory explanation the local institution offers for how the two systems relate — without those explanations being consistent across regions, sects, or even across a single household's lifetime rituals. They bear the cognitive and practical cost of holding contradictory frameworks (kami as native deities, kami as suffering beings needing salvation, kami as manifestations of buddhas) without any institution being obligated to resolve the contradiction for them. Leaving the arrangement risks loss of burial rights, social standing, and suspicion of proscribed religious affiliation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, trapped, local).

% Kami shrine priests (shinshoku) in many jingu-ji complexes were institutionally subordinated to Buddhist temple administration, required to accept a Buddhist framing of their own tradition's cosmology (kami as suffering beings, or as local traces of buddhas) as the price of institutional survival and state recognition. Their exit options are constrained: independent shrine operation outside the combinatory system exposes them to the loss of land grants and state legitimacy, though a minority of shrines (notably some Ise and Shinto-primacy lineages) retained partial autonomy through separate political leverage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests_subordinated, payer,
    moderate, generational, constrained, regional).

% Local mountain ascetics, mediums, and folk ritual specialists (yamabushi and others) operate rites that predate and exceed the official combinatory schema, but must present their practice within the sanctioned honji suijaku vocabulary to avoid suppression as unlicensed or heterodox activity. Their actual cosmological commitments are flattened into whatever administrative category keeps them legally operating.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_ritual_specialists, payer,
    powerless, biographical, trapped, local).

% Later reformers who forcibly separated kami and buddha worship (shinbutsu bunri, 1868) would argue retroactively that the fusion was never a coherent unity, only an administrative overlay to be dismantled — but they are a future faction, not a present party to the story's interval, and their own separation project imposed its own incoherent, newly invented purity claims. They are named here as evidence that even the dismantling of the arrangement was driven by state interest, not doctrinal resolution.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, meiji_era_shinbutsu_bunri_reformers, excluded,
    organized, generational, mobile, national).

% Modern scholars examining temple registers, honji suijaku treatises, and regional variation across shrine-temple complexes, documenting that the metaphysical content of the 'fusion' varied so widely by domain, sect, and period that no single doctrinal claim was ever uniformly held — the coherence was administrative and retrospective, not lived and settled.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, doctrinal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, shogunate_religious_administration).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine administrative coordination function: the danka/jingu-ji system lets the state track population, manage land and tax obligations tied to religious institutions, and surveil against proscribed religions (notably Christianity) through a single legible registration channel. This is real coordination — of state administrative capacity, not of religious belief.
% TRANSFER_FUNCTION: The arrangement moves institutional legitimacy, land grants, and tax revenue toward the shogunate's religious administration and the combinatory temple-shrine complexes, while moving the cognitive and practical burden of holding an unresolved, locally-varying cosmology onto lay practitioners, subordinated shrine priests, and local ritual specialists who must fit their practice into whatever vocabulary keeps them legally recognized.
% ABSENT_VOICES: Ordinary practitioners' actual, often locally coherent, folk cosmologies are never solicited or recorded by the administering institutions — the officially sanctioned honji suijaku vocabulary is imposed top-down, and where local practice contradicts it, the local practice is simply not part of the administrative record. Village ritual specialists whose cosmology predates the combinatory schema are structurally excluded from having their own account count as doctrine.
% DISAPPEARANCE_RATIONALE: If the state-enforced fusion apparatus disappeared, the danka registration system, jingu-ji institutional revenue, and the administrative fiction of settled kami-buddha unity would all collapse or fragment rapidly — as in fact happened, abruptly, with the Meiji shinbutsu bunri edicts of 1868, which dismantled the combinatory institutions within a few years once state interest shifted toward Shinto establishment for different political ends. The rapid, near-total institutional unwinding is itself evidence that the arrangement was administrative scaffolding, not a settled metaphysical unity that practitioners would have defended on its own terms.
% FOUNDING_PROBLEM: The Nara-through-Edo state needed a way to administer, tax, and surveil a population whose pre-existing kami worship and imported Buddhist institutions were not naturally unified, without either suppressing kami worship outright (politically costly, given its role in legitimating imperial and local authority) or leaving Buddhist institutions without a mechanism to absorb and administer local cultic practice.
% FOUNDING_PROBLEM_CORROBORATION: The abruptness and near-total success of the Meiji shinbutsu bunri separation (1868-1872), carried out by a new state faction with no stake in the old jingu-ji revenue structure, corroborates from outside the benefiting institutions that the fusion's administrative function had become obsolete to state interest and was not defended as doctrine by the shrine priests or lay population once state backing was withdrawn. Independent historical scholarship (e.g. studies of regional honji suijaku variation) further corroborates that no uniform doctrinal content existed to defend.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by interval end) is authored as substantial but not maximal: real administrative coordination value existed (population registration, anti-Christian surveillance, tax administration), so this is not pure extraction with zero coordination function — it is extraction riding on genuine state-administrative coordination, which is why claimed_type is snare rather than tangled_rope: the coordination benefit accrues almost entirely to the state and institutional beneficiaries, and no meaningfully symmetric benefit reaches the payer seats to satisfy a coordination-for-participants reading. Suppression (0.71) is high because dissenting or locally-coherent cosmologies had to be suppressed or reworded to survive administratively, and this suppression intensified over the interval as danka registration hardened from a loose expectation into compulsory universal registration. Theater ratio (0.62) is high and rising because an increasing share of the honji suijaku vocabulary's institutional function was performative — maintaining the appearance of settled doctrine for administrative and legitimation purposes — rather than reflecting any actual resolved belief structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunate administration and temple-shrine institutional beneficiaries sit near the full-beneficiary end: they collect legitimacy, revenue, and administrative control, and can walk away from any given doctrinal claim without losing what they actually need (compliance, not coherence) — hence arbitrage/analytical exit options. Lay practitioners and village ritual specialists sit near the full-target end: trapped exit options, powerless power atom, and they bear the entire cost of holding contradictory frameworks without any institutional obligation to resolve the contradiction for them. Subordinated shrine priests occupy an intermediate position — moderate power, constrained exit — because some retained partial leverage (particularly higher-status shrine lineages) even as most were pulled into Buddhist-administered subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as dead: the administrative problem the fusion solved (population legibility, anti-Christian surveillance, land/tax administration under a single registry) had a clear terminus with Meiji state modernization, which replaced danka registration with a family registry (koseki) system serving the same coordination function through wholly secular means. The arrangement's doctrinal content did not need to be true for the coordination problem to be real; conversely, the coordination problem's resolution by other (secular) means, with essentially no doctrinal defense mounted by former beneficiaries once patronage shifted, is read here as confirming the founding cosmological content was never load-bearing. This is precisely the mandatrophy the classification protects against confusing: real state coordination (registration, surveillance) misread as evidence of doctrinal coherence (kami-buddha unity), when the doctrine was scaffolding for the coordination, not the coordination's cause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_existence_ambiguity,
    'Is there in fact no coherent kernel at all (this reading), or does a coherent kernel exist at either the ontological-fusion level (syncretic_fusion_reading) or the functional-partition level (domain_partition_reading), with the appearance of incoherence merely reflecting incomplete historical documentation of a genuinely held synthesis?',
    'Systematic comparison of honji suijaku treatises and shrine-temple charter documents across regions and centuries for convergent versus divergent metaphysical content; if independent lineages converge on substantially similar accounts of kami-buddha relation without state coordination, that would support a genuine (if not fully state-independent) kernel; persistent, uncoordinated divergence supports the incoherent-bundle reading.',
    'If a genuine kernel is found, this constraint''s classification would need to shift toward tangled_rope (real shared commitment plus asymmetric extraction) or even rope; if no kernel is found across a representative sample, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_existence_ambiguity, conceptual, 'Whether the apparent absence of doctrinal coherence is real or an artifact of documentation gaps.').

omega_variable(
    reading_selection_criterion,
    'What specific historiographic signal justifies selecting the incoherent_bundle_reading over the syncretic_fusion_reading or domain_partition_reading as the operative account of this kernel for this story?',
    'The primary signal used here is the abruptness and administrative character of the Meiji shinbutsu bunri dismantling (1868-1872): a genuinely held ontological unity or functional partition would be expected to leave more doctrinal resistance or continued private practice after state withdrawal of enforcement than the historical record shows. A conceptual reading choice, not a settled empirical fact — other historians read the same dismantling as a state-imposed rupture of a real prior synthesis, which would support the syncretic_fusion_reading instead.',
    'If the alternative signal (continued vigorous private syncretic practice after 1868, independent of state institutions) were dominant in the record, the domain_partition_reading or syncretic_fusion_reading would be better supported, and this story''s high ε/suppression values would not transfer to that sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_criterion, conceptual, 'Documents the framing choice behind selecting this reading and what evidence would favor a sibling reading instead.').

omega_variable(
    practitioner_lived_coherence,
    'Even if no institutional or textual kernel is coherent across regions, did individual practitioners or local communities achieve a personally coherent synthesis that this reading, focused on institutional incoherence, underweights?',
    'Ethnographic and folklore-record analysis of specific villages'' ritual practice over multiple generations, looking for internally stable (even if locally idiosyncratic) cosmological accounts that persisted independent of the official honji suijaku vocabulary.',
    'If local coherence is found, the suppression and extraction measured here may overstate the burden on lay practitioners, who might have held workable local syntheses regardless of the incoherent official superstructure — this would lower the effective χ for the powerless stakeholder seats without changing the institutional-level classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_lived_coherence, empirical, 'Whether local/lived practice achieved coherence the official institutional bundle lacked.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the shinbutsu_ontological_substrate kernel: this incoherent_bundle_reading (snare — enforced administrative fusion with no doctrinal load-bearing content, high ε/suppression), the syncretic_fusion_reading (near-zero ε — genuine ontological unity, no enforcement needed if the metaphysics is simply true), and the domain_partition_reading (lower ε, coordination-flavored — functional division of religious labor between this-world and afterlife concerns, requiring far less suppression since it does not ask practitioners to hold literally identical/unified beliefs, only to allocate different concerns to different ritual specialists). All three describe the same historical kami-buddha combinatory institutions (jingu-ji, honji suijaku doctrine, danka registration) but attribute radically different underlying commitment structures, hence radically different ε and classification. Per the ε-invariance principle, these are not one constraint measured three ways but three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
