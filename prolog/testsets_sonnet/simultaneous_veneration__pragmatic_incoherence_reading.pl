% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Shinbutsu-shūgō as Unresolved Doctrinal Incoherence Sustained by Institutional Non-Enforcement
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   Shinbutsu-shūgō, the long syncretic fusion of kami worship and Buddhism
 *   in Japan (c. 8th century-1868), is conventionally described as coherent
 *   religious synthesis. This reading treats that description as false:
 *   practitioners at every level, from villagers to Buddhist clergy
 *   administering shrine estates, held mutually contradictory beliefs about
 *   the status of kami (final beings worthy of worship in their own right vs.
 *   suffering beings in need of Buddhist salvation vs. local manifestations
 *   of transcendent buddhas) without any institutional mechanism ever
 *   adjudicating between them. The arrangement persisted for centuries not
 *   because it worked doctrinally but because no external enforcement
 *   pressure existed to force a reckoning — the combined shrine-temple
 *   institutions had every incentive to preserve ambiguity, since ambiguity
 *   let them collect legitimacy and revenue from both cosmological registers
 *   at once. The Meiji shinbutsu-bunri edicts, in this reading, function as
 *   the arrival of exactly that enforcement pressure: a state authority with
 *   the will and capacity to force resolution, revealing incoherence that had
 *   been there all along rather than tearing apart something that had
 *   genuinely worked.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: institutional beneficiary collecting dual legitimacy and revenue from unresolved fusion
 *   - buddhist_clergy_managing_shrine_estates: institutional beneficiary whose administrative seniority depended on the fusion remaining untested
 *   - lay_practitioners_navigating_contradictory_cosmologies: primary payers bearing unresolved cognitive/ritual burden for centuries
 *   - meiji_state_officials: agenda-setters who converted latent incoherence into a lever for new state ideology
 *   - comparative_religion_scholars: analytical observers evaluating whether the historical record supports synthesis or sustained non-resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.71).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Shinbutsu-shūgō as Unresolved Doctrinal Incoherence Sustained by Institutional Non-Enforcement").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, 'e56ef6c6-b22b-424b-a73b-97a11c66ef66').
narrative_ontology:cs_kernel_codification('e56ef6c6-b22b-424b-a73b-97a11c66ef66', distributed).
narrative_ontology:cs_authority_grounding('e56ef6c6-b22b-424b-a73b-97a11c66ef66', practice).
narrative_ontology:cs_interpretation_layer_present('e56ef6c6-b22b-424b-a73b-97a11c66ef66').
narrative_ontology:cs_reading_relation('e56ef6c6-b22b-424b-a73b-97a11c66ef66', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('e56ef6c6-b22b-424b-a73b-97a11c66ef66', simultaneous_veneration__domain_partition_reading, influences).
narrative_ontology:cs_axiom('e56ef6c6-b22b-424b-a73b-97a11c66ef66', foundational, doctrinal_coherence_was_never_achieved_or_required_by_the_institutions_that_profited).
narrative_ontology:cs_axiom_status(doctrinal_coherence_was_never_achieved_or_required_by_the_institutions_that_profited, holdable).
narrative_ontology:cs_axiom_grounding('e56ef6c6-b22b-424b-a73b-97a11c66ef66', doctrinal_coherence_was_never_achieved_or_required_by_the_institutions_that_profited, empirically_contingent).
narrative_ontology:cs_axiom('e56ef6c6-b22b-424b-a73b-97a11c66ef66', foundational, enforcement_absence_not_theological_agreement_explains_persistence).
narrative_ontology:cs_axiom_status(enforcement_absence_not_theological_agreement_explains_persistence, holdable).
narrative_ontology:cs_axiom_grounding('e56ef6c6-b22b-424b-a73b-97a11c66ef66', enforcement_absence_not_theological_agreement_explains_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('e56ef6c6-b22b-424b-a73b-97a11c66ef66', unadjudicated_syncretic_accretion).
narrative_ontology:cs_drift_state('e56ef6c6-b22b-424b-a73b-97a11c66ef66', meiji_shinbutsu_bunri_1868, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('e56ef6c6-b22b-424b-a73b-97a11c66ef66', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_clergy_managing_shrine_estates).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, kami_priests_embedded_in_temple_hierarchies).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_navigating_contradictory_cosmologies).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, doctrinal_reformers_suppressed_under_syncretic_orthodoxy).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, meiji_era_shinto_purists_denied_prior_articulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, kami_priests_embedded_in_temple_hierarchies).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, coherence_is_not_required_for_institutional_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingu-ji institutions (combined shrine-temple complexes) administer both kami rites and Buddhist liturgy under one estate, collecting tribute, land income, and pilgrimage revenue from both cosmological registers simultaneously. They have no incentive to resolve the contradiction between kami-as-suffering-beings-needing-salvation and kami-as-avatars-of-buddhas, because the ambiguity lets them claim both revenue streams and both forms of legitimacy without ever being tested against a single coherent doctrine.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes, agenda_setter).

% Buddhist clerics installed as bettō (shrine administrators) absorbed kami cults into temple economies, performing sutra readings for kami and asserting doctrinal seniority (honji-suijaku) without ever having to demonstrate the metaphysical claim held under scrutiny. Their institutional position depended on the unresolved status of the fusion, not its truth.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_clergy_managing_shrine_estates, beneficiary,
    institutional, generational, arbitrage, national).

% Shrine priests operating under Buddhist institutional umbrellas retained ritual authority and income but occupied a subordinate doctrinal position (kami as manifestations rather than final beings). They benefited from continued patronage and protection but paid in reduced status whenever the fusion doctrine was invoked to rank Buddhist truth above kami practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kami_priests_embedded_in_temple_hierarchies, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, kami_priests_embedded_in_temple_hierarchies, payer).

% Villagers and townspeople venerated kami for harvest and protection and buddhas for funerary rites and afterlife salvation, without any authority ever explaining how these could be true of the same beings or cosmos at once. They bore the cognitive and ritual cost of enacting a system that no one — not the priests, not the clergy — could state coherently, because coherence was never the point of the arrangement for those who administered it.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_navigating_contradictory_cosmologies, payer,
    powerless, biographical, trapped, local).

% Figures within both Buddhist and kami traditions who argued for doctrinal clarification (proto-purist kami theorists, strict Pure Land exclusivists) were marginalized or absorbed by the combinatory institutions, which had every reason to prevent a clarifying test that would force the incoherence into the open and threaten the dual revenue base.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, doctrinal_reformers_suppressed_under_syncretic_orthodoxy, payer,
    moderate, generational, constrained, national).

% Nativist scholars (kokugaku lineage) who wanted to articulate a pure, unmixed kami tradition were denied that articulation for centuries by an institutional order that had no mechanism for resolving the underlying contradiction; when shinbutsu-bunri finally arrived in 1868, it was framed as separating two things that had been fused, when in this reading it was actually the first moment anyone was permitted to say the fusion had never made sense.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_era_shinto_purists_denied_prior_articulation, payer,
    organized, generational, constrained, national).

% State Shinto architects used shinbutsu-bunri to nationalize kami worship independent of Buddhist institutional control, seizing the historical moment when enforcement pressure against the old ambiguity finally existed. They are not victims of the incoherence but its first exploiters at scale — converting a centuries-old unresolved contradiction into a lever for a new state ideology.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_officials, agenda_setter,
    institutional, generational, arbitrage, national).

% Study shinbutsu-shūgō as a test case for whether syncretism represents genuine synthesis or sustained non-resolution; this reading treats the absence of enforcement, not the presence of theological agreement, as the operative mechanism holding the arrangement together for nearly a millennium.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement allowed a single set of ritual, economic, and administrative institutions to service two distinct devotional needs (this-worldly kami protection and Buddhist soteriology) without requiring either community to abandon or subordinate its practice outright — a genuine coordination convenience for institutions managing dual constituencies.
% TRANSFER_FUNCTION: Moves ritual authority, land income, and doctrinal legitimacy toward the combined shrine-temple administrative apparatus and its clergy, and moves cognitive and ritual burden — the cost of holding two irreconcilable cosmologies without adjudication — onto lay practitioners and onto any reformer who sought doctrinal clarity.
% ABSENT_VOICES: Lay practitioners who experienced the contradiction directly left almost no textual record of complaint — the sources are overwhelmingly clerical and administrative. Kokugaku purists who might have named the incoherence earlier were structurally excluded from doctrinal authority under the combinatory system and only gained a hearing once state power backed their position after 1868.
% DISAPPEARANCE_RATIONALE: The shinbutsu-bunri edicts of 1868 demonstrate exactly this: when the arrangement was forcibly dissolved, temple-shrine complexes were physically separated, thousands of Buddhist images were removed from shrines, priesthoods were reorganized along single-tradition lines, and an entirely new institutional order (State Shinto) emerged within a few years — confirming that the prior arrangement was load-bearing for specific institutional arrangements, not a free-floating cultural preference.
% FOUNDING_PROBLEM: Early Japanese Buddhist institutions needed to establish legitimacy and gain adherents in a landscape already saturated with kami worship; absorbing kami into a Buddhist cosmological frame (as avatars, as beings needing salvation) solved the immediate problem of religious competition without requiring displacement of existing practice.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state Shinto ideologues and kokugaku scholars (Hirata Atsutane's intellectual heirs) attested, from outside the Buddhist-administered shrine-temple complexes that had benefited from the fusion, that the doctrinal problem of establishing Buddhism's position relative to indigenous kami worship had long since been resolved by Buddhism's total institutional dominance — meaning the fusion's original competitive function was obsolete for centuries before formal separation, persisting instead as administrative and economic convenience for the institutions built on top of it.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising across the interval because the story's claim is that the fusion's incoherence was not neutral — it was structurally convenient for institutions that could collect from both registers, and that convenience compounded as combined institutions consolidated landholdings and ritual monopolies over centuries. Theater ratio is authored high and rising (0.68) because honji-suijaku theorizing, in this reading, functioned increasingly as post-hoc justification dressing rather than a genuine resolving doctrine — more elaborate scholastic apparatus arose precisely as the underlying contradiction became harder to ignore. Suppression is authored as substantial and rising (0.62) not because of violent coercion but because the absence of any adjudicating mechanism functioned as a suppression of the question itself: no venue existed where the contradiction could be raised and settled, which is a form of suppression by omission rather than by force. Accessibility collapse is authored lower (0.4) because, unlike a mountain, alternative coherent framings (strict kami-only or strict Buddhist-only positions) remained conceivable and were in fact articulated by marginalized reformers throughout the period — they simply lacked institutional purchase.
 *
 * DIRECTIONALITY LOGIC:
 *   The combined shrine-temple institutions and the Buddhist clergy administering them sit at the beneficiary end: they collected the surplus legitimacy and revenue generated by never having to choose between cosmologies, and their exit options were effectively arbitrage (they could shift emphasis toward whichever framing suited a given audience or transaction). Lay practitioners, doctrinal reformers, and the eventual Meiji-era Shinto purists sit at the target end: they bore the unresolved cost — cognitive for laypeople, institutional-exclusion for reformers — with no exit, since leaving the ambient religious culture was not a realistic option in a fully saturated devotional environment. Kami priests occupy an intermediate position: they benefited from continued patronage under the combined system but paid in subordinated doctrinal status, which is why they are marked with both beneficiary and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Buddhism needing legitimacy in a landscape already populated by kami worship) was resolved by roughly the Heian period through Buddhism's institutional dominance — long before 1868. The arrangement's persistence for another seven-plus centuries past that resolution, defended by clergy who profited from its continuation, is a textbook mandatrophy pattern: this reading holds that the fusion outlived its founding function and was maintained by institutional inertia and dual-revenue incentive rather than any live doctrinal need, until an external actor (the Meiji state) supplied the enforcement pressure that had never previously existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_synthesis_underdetermination,
    'Is the historical record genuinely more consistent with sustained, unresolved contradiction (this reading) than with either functional domain-partition or true ontological fusion — or is ''incoherence'' itself a modern analytical projection onto premodern actors who did not share our expectation that religious cosmologies be logically consistent?',
    'Close textual analysis of premodern sermons, ritual manuals, and lay testimony (where extant) for explicit or implicit acknowledgment of tension between kami and Buddhist claims, compared against records showing untroubled simultaneous practice; comparative analysis with other historical syncretic systems where actors either did or did not experience their beliefs as requiring logical reconciliation.',
    'If premodern practitioners genuinely did not experience or require logical consistency (the domain_partition_reading''s implicit claim, or a version of ontological_fusion where the honji-suijaku framework was experientially satisfying), this constraint''s high extractiveness score overstates a burden that was not actually borne — the ''cost'' of incoherence may be an anachronistic import from a post-Enlightenment expectation of doctrinal consistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_synthesis_underdetermination, conceptual, 'Whether incoherence is a genuine historical burden or a modern analytical artifact projected backward.').

omega_variable(
    meiji_rupture_vs_revelation,
    'Did shinbutsu-bunri reveal a pre-existing incoherence (this reading) or did it manufacture a rupture in a system that had achieved genuine, if informal, stability — with the ''incoherence'' narrative itself serving Meiji state ideology''s need to portray the prior arrangement as always having been illegitimate?',
    'Examination of Meiji-era state rhetoric for whether it explicitly framed the prior fusion as incoherent (supporting this reading) versus framing it as a foreign corruption to be purged (a different rhetorical strategy consistent with either sibling reading being retrospectively delegitimized rather than diagnosed as incoherent); comparison with local-level resistance to the 1868 edicts, which would suggest communities experienced the prior arrangement as coherent and worth defending.',
    'If local resistance to shinbutsu-bunri was widespread and articulate in defending the coherence of the prior system, this reading''s central claim (that no one could state the fusion coherently) is undermined, and the domain_partition_reading or ontological_fusion_reading gains support as the historically operative belief structure for at least some communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_vs_revelation, empirical, 'Whether Meiji separation reveals latent incoherence or manufactures a rupture narrative serving state ideology.').

omega_variable(
    beneficiary_capture_of_doctrinal_ambiguity,
    'Was the doctrinal ambiguity a deliberate institutional strategy pursued by shrine-temple complexes to maximize dual revenue capture, or an unintended byproduct of gradual historical accretion with no single agent ever choosing ambiguity as a strategy?',
    'Search institutional records (temple ledgers, land grant documents, clerical correspondence) for explicit discussion of the benefits of maintaining ambiguous doctrinal status, versus records showing genuine, unstrategic confusion or good-faith attempts at reconciliation that simply failed to gain traction.',
    'If ambiguity was unintended accretion rather than strategy, the tangled_rope classification (which requires identifiable beneficiaries actively sustaining an extractive structure) weakens toward a piton reading — persistence by inertia with no one meaningfully steering the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_doctrinal_ambiguity, empirical, 'Whether institutional beneficiaries strategically sustained ambiguity or merely inherited it without agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(simu_tr_t0, projected).
narrative_ontology:measurement(simu_tr_t300, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 300, 0.48).
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 700, 0.55).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1100, 0.6).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1500, 0.65).
narrative_ontology:measurement(simu_tr_t1750, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1750, 0.68).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1868, 0.68).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(simu_be_t0, projected).
narrative_ontology:measurement(simu_be_t300, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 700, 0.55).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1100, 0.62).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(simu_be_t1750, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1750, 0.71).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1868, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(simu_su_t0, projected).
narrative_ontology:measurement(simu_su_t300, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 700, 0.47).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1100, 0.53).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement(simu_su_t1750, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1868, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the simultaneous_veneration kernel, each authored as an independent, ε-invariant constraint per the ε-invariance principle. domain_partition_reading treats kami and buddhas as functionally distinct entities governing separate life-domains, with simultaneous veneration as legitimate specialization (low extraction, rope-adjacent). ontological_fusion_reading treats honji-suijaku as capturing genuine metaphysical identity between kami and buddhas (low-to-moderate extraction, closer to rope or mountain depending on how strongly the metaphysical claim is held). This reading (pragmatic_incoherence_reading) treats the arrangement as never having been doctrinally resolved at all, sustained by absent enforcement rather than truth or functional partition (high extraction, tangled_rope). The three do not average into one verdict on shinbutsu-shūgō; they are three distinct structural claims about the same historical label, linked here for contamination and family-tracing purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
