% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Article II Extraction-Permissive Reading: Sovereign Claims Barred, Private Resource Title Permitted
 *   domain: international law/space governance/commons
 *
 * SUMMARY:
 *   This story instantiates the extraction_permissive reading of the Article
 *   II non-appropriation kernel (Outer Space Treaty, 1967): the clause bars
 *   national appropriation 'by claim of sovereignty, by means of use or
 *   occupation, or by any other means,' and this reading holds that it bars
 *   sovereign territorial claims only — private ownership of extracted
 *   resources is outside the prohibition and therefore permitted by default
 *   (the Lotus inference). Since 2015 the reading has become operative law:
 *   US Commercial Space Launch Competitiveness Act Title IV (2015),
 *   Luxembourg's space-resources law (2017), UAE decree (2019), Japan's act
 *   (2021), and the Artemis Accords' resource provisions (2020). The epsilon
 *   referent is the standing arrangement under contest as this reading
 *   maintains it: resource access gated by technological capability and
 *   flag-state legal recognition, no compensation mechanism for excluded
 *   states, enclosure proceeding by fait accompli (exclusive licenses, safety
 *   zones) rather than formal annexation. The reading considers this
 *   arrangement lawful utilization; the structural facts it accepts — the
 *   capability gate, the absent return flow — still register as high
 *   extraction from a commons declared 'the province of all mankind.' Sibling
 *   readings (commons_conservation, international_regime) are separate
 *   constraints with their own epsilon; this story is the epsilon-invariant
 *   account of the permissive reading alone.
 *
 * KEY AGENTS:
 *   - extraction_capable_flag_states: agenda-setter (institutional/arbitrage) — enacts permissive national law, licenses operators, extends the reading via the Artemis coalition; also collects industrial-policy gains
 *   - commercial_extraction_operators: primary beneficiary (powerful/arbitrage) — holds exclusive extraction rights under flag-state law; the seat the extraction's gains accrue to
 *   - non_spacefaring_developing_states: primary target (powerless/trapped) — capability-excluded from the commons; no compensation flows to them
 *   - moon_agreement_signatory_states: target (moderate/constrained) — endorsed the conservation reading; their path is being mooted by fait accompli
 *   - emerging_spacefaring_states: dual-positioned beneficiary/target (organized/mobile) — protected by non-appropriation today, exposed to lock-out tomorrow
 *   - russian_chinese_lunar_programs: excluded contestor (institutional/arbitrage) — outside the Artemis legal conversation; building the ILRS as a rival structure
 *   - un_copuos_diplomats: analytical observer (institutional/analytical) — hosts the space-resources working group without binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.72).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.55).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Extraction-Permissive Reading: Sovereign Claims Barred, Private Resource Title Permitted").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international law/space governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'aa148be9-dfa0-459c-bf0f-f6c21ad7263a').
narrative_ontology:cs_kernel_codification('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', fixed_text).
narrative_ontology:cs_authority_grounding('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', practice).
narrative_ontology:cs_interpretation_layer_present('aa148be9-dfa0-459c-bf0f-f6c21ad7263a').
narrative_ontology:cs_reading_relation('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', foundational, treaty_silence_permits_private_resource_rights).
narrative_ontology:cs_axiom_status(treaty_silence_permits_private_resource_rights, holdable).
narrative_ontology:cs_axiom_grounding('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', treaty_silence_permits_private_resource_rights, conventional).
narrative_ontology:cs_axiom('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', foundational, resource_extraction_distinct_from_appropriation).
narrative_ontology:cs_axiom_status(resource_extraction_distinct_from_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', resource_extraction_distinct_from_appropriation, conventional).
narrative_ontology:cs_axiom('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', secondary, flag_state_licensing_confers_resource_title).
narrative_ontology:cs_axiom_status(flag_state_licensing_confers_resource_title, holdable).
narrative_ontology:cs_axiom_grounding('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', flag_state_licensing_confers_resource_title, conventional).
narrative_ontology:cs_reference_frame('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', treaty_silence_as_permission).
narrative_ontology:cs_drift_state('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', contemporary_flag_state_practice_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('aa148be9-dfa0-459c-bf0f-f6c21ad7263a', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, commercial_extraction_operators).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, extraction_capable_flag_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, emerging_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_developing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_signatory_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, emerging_spacefaring_states).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, lotus_permissive_inference).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, first_capture_resource_title_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, flag_state_licensing_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact national laws declaring extracted space resources the property of their licensed citizens (US CSLCA Title IV 2015, Luxembourg 2017, UAE 2019, Japan 2021), license extraction missions, and extend the reading through the Artemis Accords coalition. They collect licensing revenue, industrial-policy gains, and first-mover legal position. Their exit is arbitrage-grade: they wrote the rules and can amend their own frameworks or forum-shop at will.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, extraction_capable_flag_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, extraction_capable_flag_states, beneficiary).

% Hold exclusive extraction rights and resource title under flag-state law; the reading routes ownership of extracted material directly to them. They select incorporation jurisdictions by regulatory permissiveness and lobby for the reading's extension. Their committed capital (lunar ISRU demonstrations, asteroid prospecting) depends on the title their flag states confer.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, commercial_extraction_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Have no independent access to celestial-body resources and no seat in the flag-state licensing structure. They bear the enclosure of a commons declared the 'province of all mankind': no compensation, no revenue-sharing, and a rule-making conversation that proceeds without them. Their recourse is COPUOS statements that do not bind the capable coalition.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_developing_states, payer,
    powerless, generational, trapped, global).

% Formally endorsed the common-heritage conservation path (Moon Agreement 1979, in force 1984) and now watch that path mooted as permissive national law accumulates. They retain COPUOS voice and voting presence but cannot alter the de facto regime; several are minor space powers with no extraction capability on the horizon.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_signatory_states, payer,
    moderate, generational, constrained, global).

% Operate growing space programs (lunar missions, launch capability) without near-term extraction capability. They are protected by the non-appropriation function — no rival may claim territory over their missions — and hold option value on future resource access, but they face a regime being locked in around them: rules written by the capable coalition, safety zones accumulating ahead of their arrival. They can align with Artemis or with the rival ILRS bloc.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, emerging_spacefaring_states, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, emerging_spacefaring_states, payer).

% Declined Artemis accession and stand outside the flag-state legal conversation setting the de facto resource rules. They contest the reading diplomatically and are building the International Lunar Research Station as a parallel structure with its own operating norms — exit into a rival regime rather than participation in this one.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, russian_chinese_lunar_programs, excluded,
    institutional, generational, arbitrage, global).

% Chair the COPUOS working group on space resources and host the consensus conversation that has so far produced no binding instrument. They take every seat's position on record; their output (guidelines, candidate principles) lacks enforcement force against the flag-state licensing structure.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, un_copuos_diplomats, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, commercial_extraction_operators).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents sovereign territorial claims over celestial bodies, averting annexation competition and territorial conflict in space; provides a registry and flag-state framework under which any state's missions operate without encountering a rival sovereignty claim.
% TRANSFER_FUNCTION: Moves exclusive access to celestial-body resources from a commons open in principle to all states to capability-gated actors holding flag-state licenses; extracted material and its value flow to licensed operators with no compensating flow to excluded parties.
% ABSENT_VOICES: Non-spacefaring developing states and Moon Agreement signatories object in COPUOS but lack capability leverage; the Group of 77's common-heritage position is on record but cannot bind the Artemis coalition. Future generations of commons users have no seat anywhere. The Artemis consultation process included only invited capable states.
% DISAPPEARANCE_RATIONALE: If the extraction-permissive arrangement (national title laws, licensing machinery, Artemis resource provisions) vanished overnight, committed extraction capital would freeze pending legal clarification, the flag-state laws would be inoperative, COPUOS would become the sole live venue with the question reopened, and the ILRS bloc's parallel structure would gain relative ground — the space-resource economy would reorganize around whichever reading fills the vacuum.
% FOUNDING_PROBLEM: The 1967 treaty was built to stop the Cold War territorial scramble from extending to the Moon and planets: Article II bars national appropriation 'by claim of sovereignty, by means of use or occupation, or by any other means.'
% FOUNDING_PROBLEM_CORROBORATION: The Moon Agreement's common-heritage clause (1979) and successive COPUOS space-resources working-group records attest that appropriation concern stayed live after 1967; academic international-law scholarship and the ILA space-law committee document the live dispute over whether extraction is 'use' or 'appropriation.' No source outside the beneficiary set attests that the founding problem is dead; the flag states themselves concede the contest by drafting Article 11 of the Artemis Accords to reaffirm non-appropriation.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because access to celestial-body resources is gated on capability and flag-state recognition and the arrangement contains no compensation or benefit-sharing mechanism: value flows from a commons open in principle to all states to a small set of licensed actors, with no return flow. Suppression (0.55) is structural rather than coercive: excluded states are not threatened, but their endorsed alternative (Moon Agreement common heritage) requires ratification by the very states that will never give it, and each year of fait accompli accumulation raises the cost of the deferral alternative — alternatives collapse as vested rights harden, which is what accessibility_collapse 0.52 records. Theater_ratio 0.33 and rising: safety zones disclaim sovereignty while functioning as exclusive surface control, and Artemis Article 11 reaffirms non-appropriation in the same instrument whose resource section permits extraction. Resistance 0.62: the Moon bloc, Group of 77 statements, Russia/China's rejection of Artemis and construction of the ILRS, and sustained academic international-law critique. The three measurement series share one grid (1967, 1979, 1984, 1997, 2015, 2020, 2025) with every tracked metric authored at every point; suppression_requirement is tracked because the enforcement picture genuinely changed — no licensing machinery existed before 2015, and national regulatory build-out plus coalition discipline followed.
 *
 * PERSPECTIVAL GAP:
 *   From the flag-state seat the arrangement is lawful utilization of unowned materials under a treaty that bars only sovereignty claims — the non-appropriation clause is honored, extraction is use, and national licensing fills a gap the treaty left open. From the capability-excluded seats the same structure is enclosure of a commons by those who can take from it, with the treaty clause functioning as a shield: because no sovereignty is claimed, no annexation-style challenge is available, and the enclosure proceeds without the legal contest that formal claims would trigger. Emerging spacefaring states hold both views at once — protected by the clause today, exposed to a locked-in regime tomorrow. Coalition note: the powerless seats' natural coalition (Moon bloc plus Group of 77) exists on paper but lacks capability leverage; the coalition that actually formed is the capable one (Artemis), and the rival coalition (ILRS) is a capability response, not a legal one. The engine computes per-seat classifications from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   extraction_capable_flag_states sit nearest the beneficiary end (d approximately 0.05): they wrote the gate, administer it, and collect industrial-policy and licensing gains. commercial_extraction_operators (d approximately 0.1) hold arbitrage-grade exit — they shop for the most permissive flag — which pins them near the beneficiary end despite bearing mission costs. non_spacefaring_developing_states sit nearest the target end (d approximately 0.95): trapped, capability-excluded, no compensation. moon_agreement_signatory_states (d approximately 0.75) are constrained rather than trapped — they keep COPUOS voice but watch their endorsed path mooted. One directionality override: emerging_spacefaring_states hold a beneficiary role, so structural derivation would place them near d approximately 0.2; their true position is near-symmetric (override 0.45 on the organized power atom, which only they hold in this story) because the regime is being locked in around them, they are absent from the rule-setting conversation, and safety zones accumulate ahead of their arrival.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stopping a territorial scramble — is not dead: no state has claimed territory, and the clause still does that work. What has grown inside the clause's shell is a successor problem the 1967 drafters did not face: capability-gated enclosure without compensation. Classification as tangled_rope (not snare) preserves what the victims themselves receive — the anti-annexation function protects every state's missions, including the excluded. Classification as tangled_rope (not rope) preserves the asymmetric transfer the coordination function now rides on. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is load-bearing today even though the parties dispute whether it still solves its founding problem or has replaced it. Mandatrophy is not declared: the mandate has not clearly outlived its function — it has been extended into a function its drafters did not author.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the extraction-permissive reading the correct instantiation of the Article II non-appropriation kernel, or do the commons-conservation and international-regime readings better capture the treaty''s object and purpose?',
    'An ICJ advisory opinion, a binding COPUOS consensus instrument on space resources, or long-run crystallization of state practice with absence of protest would settle which reading holds the kernel.',
    'If the conservation reading prevails, current operator titles void, the beneficiary set inverts into a victim set, and this story''s epsilon referent becomes an unlawful-enclosure arrangement; if the deferral reading prevails, this story''s classification is provisional until a multilateral regime supersedes national law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This story is one of three readings of the Article II kernel; the sibling readings would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    fait_accompli_appropriation_boundary,
    'Does capability-gated extraction under exclusive license, combined with safety-zone practice, constitute de facto appropriation that Article II''s ''by any other means'' language reaches — or does it remain lawful use?',
    'Observe whether safety zones harden into exclusive surface control over mission-relevant terrain and whether a zone ever excludes another state''s mission; the boundary is tested at first exclusion and at any capable-state acceptance of adjudication.',
    'If the practice crosses the appropriation boundary, the reading''s foundational axiom is contradicted by its own operation and the constraint drifts toward snare with the flag states as exposed violators; if it stays within use, the coordination function remains dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_appropriation_boundary, empirical, 'Whether the reading''s operational practice contradicts its own textual premise.').

omega_variable(
    compensation_mechanism_emergence,
    'Will a compensation or benefit-sharing mechanism (the Article XI analogue the deferral reading anticipates) arrive before enclosure is complete?',
    'Track COPUOS space-resources working-group output and any Artemis-side willingness to negotiate benefit-sharing; the mechanism either appears as a binding instrument or does not by the time extraction reaches commercial scale.',
    'A compensation mechanism would damp effective extraction for the excluded seats and stabilize the tangled-rope structure with side-payments; continued absence pushes drift toward snare as the no-compensation structure hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_mechanism_emergence, empirical, 'Whether the arrangement acquires a return flow to excluded parties in time.').

omega_variable(
    capability_gate_diffusion,
    'Will extraction capability diffuse across many states (making the capability gate transitional) or concentrate further (making it permanent)?',
    'Track the number of states with independent lunar/asteroid ISRU capability and the licensing concentration of extraction missions over the coming decades.',
    'Diffusion converts the gate into a temporary coordination device (rope-ward drift; the victim set shrinks); concentration entrenches the two-tier structure (snare-ward drift; the victim set expands to all late-arrivers).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_gate_diffusion, empirical, 'Whether the access gate is transitional or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_ii_permissive_tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.08).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t1967, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t1979, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1979, 0.1).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t1979, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t1984, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1984, 0.12).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t1984, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t1997, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1997, 0.15).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t1997, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.22).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t2015, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t2020, observed).
narrative_ontology:measurement(ost_ii_permissive_tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.33).
narrative_ontology:measurement_basis(ost_ii_permissive_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ost_ii_permissive_be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.12).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t1967, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t1979, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1979, 0.18).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t1979, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t1984, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1984, 0.24).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t1984, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t1997, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1997, 0.33).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t1997, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t2015, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t2020, observed).
narrative_ontology:measurement(ost_ii_permissive_be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(ost_ii_permissive_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost_ii_permissive_su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.15).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t1967, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t1979, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1979, 0.22).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t1979, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t1984, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1984, 0.26).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t1984, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t1997, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1997, 0.32).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t1997, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t2015, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t2020, observed).
narrative_ontology:measurement(ost_ii_permissive_su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(ost_ii_permissive_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% The natural-language label 'Article II non-appropriation' covers three structurally distinct claims (epsilon-invariance decomposition): the permissive reading (this story — high epsilon, capability-gated access, no compensation), the conservation reading (de facto appropriation barred; different victim set and epsilon), and the deferral reading (question open pending a multilateral regime). This story links to both siblings. The permissive reading exerts upstream structural pressure on the deferral sibling: every year of fait accompli accumulation (licenses, safety zones, committed capital) raises the cost of any future regime and changes its legitimacy conditions, which is why the influence edge runs from this story to international_regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
