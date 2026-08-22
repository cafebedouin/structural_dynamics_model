% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Secular-Nationalist Reading of the Turkish Graphemic Substrate (1928 Latin Script Settlement)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   On 1 November 1928 the Turkish Republic made Latin script the sole lawful
 *   substrate of public writing, replacing the Ottoman-Arabic alphabet within
 *   months rather than years. The move solved a real problem — an Arabic
 *   script poorly matched to Turkish vowels atop roughly ninety percent
 *   illiteracy — and simultaneously executed a civilizational operation:
 *   cutting the population's living connection to its Ottoman-Islamic textual
 *   inheritance and re-keying national identity to Europe. Enforcement was
 *   total and fast: night schools, fines for Arabic-script signage and
 *   ledgers, closure of Arabic-script public print. Over the following
 *   century the settlement normalized into voluntary compliance while its
 *   identity-engineering layer stayed politically alive, resurfacing whenever
 *   the state's secularist or religiously-inflected coalitions traded places.
 *   This story authors ONE reading of the contested
 *   turkish_graphemic_substrate kernel — the secular_nationalist_reading — as
 *   a clean, epsilon-invariant constraint; the ottoman_continuity_reading and
 *   gradual_transition_reading are separate files with their own structures.
 *   The epsilon referent is the standing Latin-script settlement as this
 *   reading holds and maintains it, assessed by the reading's own lights. KEY
 *   AGENTS (by structural relationship): - republican_state_apparatus:
 *   agenda-setter and primary beneficiary (institutional/arbitrage) — sets,
 *   enforces, and collects - kemalist_intelligentsia: aligned beneficiary
 *   (powerful/identity_locked) — the project's authors, fused to it -
 *   ottoman_script_literates: primary target (moderate/trapped) — bears the
 *   rupture cost directly - islamic_religious_establishment: target
 *   (organized/constrained) — loses its script-carried authority -
 *   anatolian_peasant_new_literates: dual-positioned (powerless/trapped) —
 *   gains literacy, bears homogenization - kurdish_minority_speakers: target
 *   (powerless/trapped) — bears the alphabet's assimilation edge -
 *   gradualist_educators: excluded voice (moderate/mobile) — the
 *   managed-transition alternative, sidelined - international_orientalists:
 *   analytical observer (moderate/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.64).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.45).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Secular-Nationalist Reading of the Turkish Graphemic Substrate (1928 Latin Script Settlement)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '8f799a5f-c130-4e56-b321-7c22b0dcb224').
narrative_ontology:cs_kernel_codification('8f799a5f-c130-4e56-b321-7c22b0dcb224', formalized).
narrative_ontology:cs_authority_grounding('8f799a5f-c130-4e56-b321-7c22b0dcb224', extraction).
narrative_ontology:cs_interpretation_layer_present('8f799a5f-c130-4e56-b321-7c22b0dcb224').
narrative_ontology:cs_reading_relation('8f799a5f-c130-4e56-b321-7c22b0dcb224', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8f799a5f-c130-4e56-b321-7c22b0dcb224', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('8f799a5f-c130-4e56-b321-7c22b0dcb224', foundational, turkish_identity_distinct_from_ottoman_islamic_past).
narrative_ontology:cs_axiom_status(turkish_identity_distinct_from_ottoman_islamic_past, holdable).
narrative_ontology:cs_axiom_grounding('8f799a5f-c130-4e56-b321-7c22b0dcb224', turkish_identity_distinct_from_ottoman_islamic_past, conventional).
narrative_ontology:cs_axiom('8f799a5f-c130-4e56-b321-7c22b0dcb224', foundational, latin_script_is_european_modernity_substrate).
narrative_ontology:cs_axiom_status(latin_script_is_european_modernity_substrate, holdable).
narrative_ontology:cs_axiom_grounding('8f799a5f-c130-4e56-b321-7c22b0dcb224', latin_script_is_european_modernity_substrate, instrumental).
narrative_ontology:cs_reference_frame('8f799a5f-c130-4e56-b321-7c22b0dcb224', latin_modernity_settlement).
narrative_ontology:cs_drift_state('8f799a5f-c130-4e56-b321-7c22b0dcb224', neo_ottoman_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f799a5f-c130-4e56-b321-7c22b0dcb224', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_intelligentsia).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_peasant_new_literates).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literates).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, islamic_religious_establishment).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_peasant_new_literates).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, phonetic_script_literacy_hypothesis).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, national_identity_engineering_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, civilizational_realignment_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Alphabet Law of November 1928, ran the Millet Mektepleri night-school campaigns, fined officials and merchants who kept Arabic-script signs and ledgers, and shut down Arabic-script printing in the public sphere. Gains administrative legibility, a secularization lever over schooling and religion, and European diplomatic credibility. Sets the rules it also lives under, so it can reposition at will.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The European-aligned bureaucratic and military elite whose cultural program the settlement realizes. Careers, reputations, and self-concept are fused with the reform; abandoning it would mean disowning their life's work. They receive a nation remade in their image and staff every institution that administers the script regime.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_intelligentsia, beneficiary,
    powerful, generational, identity_locked, national).

% Generations trained in Ottoman-Arabic script — scribes, clerks, teachers, poets, merchants — whose literacy became professionally worthless within months of the law. They lost access to the textual record they alone could read and had to relearn from scratch in adulthood or withdraw from literate life. There is no exit from their own generation's training.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literates, payer,
    moderate, biographical, trapped, national).

% Lost the script that carried its institutional authority: medrese teaching, sermons, religious publishing, and Quranic pedagogy all operated in Arabic letters, now severed from state schooling and public print. Organized enough to resist, but operating after the caliphate's abolition inside a state hostile to its claims; its opposition was marginalized rather than accommodated.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, islamic_religious_establishment, payer,
    organized, generational, constrained, national).

% Attended compulsory village night schools and acquired phonetic literacy far faster than the old script allowed, gaining access to state schooling, print, and administration for the first time. Bore the disruption of mandatory classes, the erasure of locally transmitted religious and customary texts, and the obligation to become literate in the state's chosen identity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_peasant_new_literates, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_peasant_new_literates, payer).

% Subjected to the same alphabet as an instrument of linguistic homogenization: Kurdish was barred from the Latin letters q, w, and x until 2013, village names and publications were suppressed, and mother-tongue schooling was denied. The alphabet that promised modernity to Turkish speakers delivered assimilation pressure to them; leaving the territory means leaving home.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_speakers, payer,
    powerless, generational, trapped, regional).

% Educators and deputies who argued through 1924-1928 for a managed five-to-fifteen-year dual-script transition preserving intergenerational knowledge transfer. Sidelined when the single-stroke law passed; their proposal survives only as a rival position, and several left public life or the country.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, gradualist_educators, excluded,
    moderate, biographical, mobile, national).

% Foreign and diaspora scholars of Ottoman and Turkic studies who documented the reform as it happened: they attest both the striking literacy gains and the abrupt severance of a living society from its manuscript culture. They bear none of the settlement's costs and collect none of its domestic benefits.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, international_orientalists, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single phonetic writing system matched to spoken Turkish's vowel inventory, enabling rapid mass literacy, uniform administration, printing, schooling, and signage across the national territory.
% TRANSFER_FUNCTION: Moves graphemic legitimacy and cultural continuity from the Ottoman-Islamic literate tradition to the republican state and its European-aligned identity project; moves the costs of adult relearning and of heritage disconnection onto existing literates, the religious establishment, and minority-language speakers.
% ABSENT_VOICES: Ottoman-script literates, religious scholars, Kurdish-speaking communities, and the deputies who proposed a managed multi-year transition stood outside the decision; the Assembly's near-unanimous vote reflected the prior marginalization of opposition seats rather than consent among those bearing the rupture costs.
% DISAPPEARANCE_RATIONALE: If the Latin mandate vanished overnight, schooling, publishing, signage, administration, and digital text would reorganize around restored script pluralism; the archive relationship between Turkish society and its pre-1928 written record would reopen; and minority-language communities would reclaim alphabetic space the settlement reserved for Turkish.
% FOUNDING_PROBLEM: Roughly nine in ten inhabitants of the new republic could not read or write; the Arabic script fitted Turkish vowel harmony poorly; and the state sought a secular national identity legible to Europe and discontinuous with the caliphate order it had just abolished.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary literacy surveys and foreign educational missions — sources outside the benefiting parties — corroborate the literacy emergency as real. Historians working outside the Kemalist tradition, on the reform-debate record, attest the identity-engineering motive behind the chosen speed. No source outside the benefiting parties attests that months-scale rupture was necessary rather than chosen.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. I claim tangled_rope because the arrangement possesses BOTH a genuine coordination function — a phonetic script demonstrably suited to Turkish, delivering one of the century's fastest literacy expansions — AND asymmetric extraction: the speed and totality of the switch exceeded any coordination need, severing a society from its own archive, dismantling a literate class's capital overnight, and later arming the alphabet against Kurdish literacy. The metrics describe that mixed operation: extractiveness 0.64 (substantial but discounted by the real literacy dividend), suppression 0.45 (active enforcement has decayed to near-voluntary compliance, but structural exclusion of the alternative script from official domains persists, and Kurdish letters were criminalized into the 2000s), theater_ratio 0.36 (the daily function is real; a growing commemorative apparatus — anniversary rites, museum narratives — performs the revolution for audiences who no longer remember the before), accessibility_collapse 0.78 (alternatives collapsed almost completely in practice; Ottoman script survives only in marginal religious, scholarly, and diasporic niches — high, but short of natural-law totality since private and academic use persisted), resistance 0.42 (real passive non-compliance, religious opposition, and dissenting deputies, crushed or marginalized quickly rather than accommodated). The measurement series run on ONE shared time grid (points 0, 10, 25, 45, 65, 80, 95) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity change — machinery built to a 1930s peak (0.76), relaxed in the multi-party 1950s, re-tightened during the 1980s securitization (0.58), then decaying again — which the static scalar cannot represent. Extractiveness dips mid-interval as the relearning burden completes and the literacy dividend matures, then climbs again as the heritage severance compounds with the dying-out of the last fluent Ottoman readers and the alphabet's minority-facing deployments.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter and intelligentsia seats the settlement is a founding achievement they administer and identify with — coordination they built, at the price they chose for others. From the trapped payer seats — Ottoman-script literates, the religious establishment, Kurdish speakers — the same structure operates as an imposed severance: their skill, authority, or tongue was repriced to zero by decree. The new-literate peasantry sits nearest symmetric: genuine access gained, genuine inheritance erased. The engine derives these divergent per-seat classifications from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the state apparatus and intelligentsia (the settlement subsidizes both; the state additionally holds arbitrage-grade exit since it writes the rules). Victim declarations drive high directionality for the three payer groups, amplified by trapped exits: the Ottoman literates cannot exit their own generation's training, the religious establishment cannot relocate its authority's carrier, Kurdish speakers cannot exit their homeland. The peasantry's dual declaration (beneficiary with payer secondary) places it mid-range — the derivation reads both flows. The excluded gradualist educators and the analytical observers contribute no directional flow; their structural significance is the suppressed alternative and the external vantage, respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was compound: a literacy emergency (widely attested solved — the arrangement's coordination half earned its keep) and an identity-construction project (still live and disputed — the arrangement's engineering half never retired). Because the status is contested rather than dead, the dead-mandate-plus-world-rearranges mismatch flag does not fire; this is not a zombie institution. The rising theater_ratio is monitored but stays below piton range: commemoration grows as living memory fades, yet the daily function — a whole society reading and writing in Latin script — remains fully operative, so performance overlays function rather than replacing it. The classification guards against two symmetrical misreadings: calling the settlement a snare erases the largest literacy dividend of the century and slanders a real coordination achievement; calling it a rope launders a coerced civilizational rupture and the deliberate destruction of a literate class's capital into mere transaction cost. Tangled rope is the honest cell: coordinated and extracted through the same stroke of the pen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_delta,
    'Is the Latin-script settlement separable from the rupture-with-the-Ottoman-Islamic-past premise, or does the secular-nationalist reading make the civilizational rupture constitutive of the graphemic constraint itself?',
    'Counterfactual comparison against the gradual_transition_reading''s managed-transition path: cases where script change proceeded without identity rupture would isolate the rupture premium inside this reading''s epsilon.',
    'If separable, the extraction attributable to identity engineering shrinks and the computed type trends toward rope; if constitutive, the extraction is intrinsic to this reading, and the sibling readings instantiate genuinely different constraints with different victim sets rather than variants of one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_delta, conceptual, 'Whether the rupture premise is constitutive or separable within this reading of the kernel.').

omega_variable(
    literacy_dividend_attribution,
    'How much of the post-1928 literacy increase is attributable to the script change itself versus the concurrent massive investment in state schooling?',
    'Comparative literacy-trajectory analysis against Arabic-script literacy campaigns elsewhere, adjusted for schooling expenditure, urbanization, and compulsory-education timing.',
    'Sizes the genuine coordination dividend — the rope component of the hybrid. A small script-specific effect would push the balance toward pure extraction riding on general educational investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_dividend_attribution, empirical, 'Attribution of the literacy gains between script design and schooling investment.').

omega_variable(
    heritage_severance_permanence,
    'Is the loss of mass access to the Ottoman textual corpus a sunk cost of the transition, or an ongoing compounding extraction as the fluent-reader population dies out?',
    'Track the demographic decay of fluent Ottoman-script readers alongside OCR and digitization recovery rates for the Ottoman corpus.',
    'If machine reading restores access, effective extraction declines over time and the settlement''s ledger improves retroactively; if the reader population collapses before recovery matures, the severance is permanent and the extraction is locked into the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_severance_permanence, empirical, 'Permanence versus recoverability of the archive severance.').

omega_variable(
    minority_weaponization_boundary,
    'Does this reading''s constraint include the alphabet''s later deployment against Kurdish literacy (the q/w/x prohibition until 2013), or is that a distinct downstream constraint?',
    'Causal-trace analysis from the 1928 Alphabet Law through the 1930s homogenization decrees to the late-twentieth-century enforcement record, distinguishing settlement logic from later security policy.',
    'Inclusion raises epsilon and widens the victim set for this story; exclusion assigns that extraction to the downstream constraint and keeps this story''s referent fixed on the 1928 settlement proper.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_weaponization_boundary, conceptual, 'Boundary between the settlement''s own extraction and its downstream minority-facing deployments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(turk_tr_t0, observed).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(turk_tr_t10, observed).
narrative_ontology:measurement(turk_tr_t25, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(turk_tr_t25, observed).
narrative_ontology:measurement(turk_tr_t45, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(turk_tr_t45, observed).
narrative_ontology:measurement(turk_tr_t65, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 65, 0.31).
narrative_ontology:measurement_basis(turk_tr_t65, observed).
narrative_ontology:measurement(turk_tr_t80, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement_basis(turk_tr_t80, observed).
narrative_ontology:measurement(turk_tr_t95, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 95, 0.36).
narrative_ontology:measurement_basis(turk_tr_t95, observed).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(turk_be_t0, observed).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(turk_be_t10, observed).
narrative_ontology:measurement(turk_be_t25, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(turk_be_t25, observed).
narrative_ontology:measurement(turk_be_t45, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement_basis(turk_be_t45, observed).
narrative_ontology:measurement(turk_be_t65, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 65, 0.6).
narrative_ontology:measurement_basis(turk_be_t65, observed).
narrative_ontology:measurement(turk_be_t80, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(turk_be_t80, observed).
narrative_ontology:measurement(turk_be_t95, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 95, 0.64).
narrative_ontology:measurement_basis(turk_be_t95, observed).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(turk_su_t0, observed).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(turk_su_t10, observed).
narrative_ontology:measurement(turk_su_t25, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(turk_su_t25, observed).
narrative_ontology:measurement(turk_su_t45, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement_basis(turk_su_t45, observed).
narrative_ontology:measurement(turk_su_t65, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 65, 0.58).
narrative_ontology:measurement_basis(turk_su_t65, observed).
narrative_ontology:measurement(turk_su_t80, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 80, 0.46).
narrative_ontology:measurement_basis(turk_su_t80, observed).
narrative_ontology:measurement(turk_su_t95, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 95, 0.38).
narrative_ontology:measurement_basis(turk_su_t95, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, gradual_transition_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_graphemic_exclusion).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_lexical_purification).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Turkish script reform' conflates the script-choice question (shared ground across all three readings) with the civilizational-rupture question (this reading's distinctive load). Measuring the settlement by literacy output yields low epsilon; measuring it by heritage severance yields high epsilon — different observables, therefore different constraints, therefore separate files. This story is the downstream member carrying the rupture load; the upstream literacy-and-modernization claim is what the rupture claim cites as its justification. Sibling readings (ottoman_continuity_reading, gradual_transition_reading) and downstream deployments (kurdish_graphemic_exclusion, turkish_lexical_purification) link here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
