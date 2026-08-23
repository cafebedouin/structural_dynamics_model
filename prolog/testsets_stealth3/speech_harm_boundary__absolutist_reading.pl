% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection with Extreme Harm Override Threshold (Absolutist Reading)
 *   domain: constitutional law/political philosophy/communication ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading, the absolutist_reading, of the
 *   contested kernel speech_harm_boundary: the settlement under which speech
 *   protection operates near-absolutely and the harm override threshold sits
 *   extremely high, leaving only a narrow unprotected set (incitement to
 *   imminent lawless action, true threats, defamation above demanding
 *   thresholds, obscenity). Per the epsilon-invariance principle, the
 *   colloquial label 'free speech versus harm' decomposes into three
 *   structurally distinct constraints, this reading plus the
 *   harm_balancing_reading and the dignity_reading, each with its own
 *   epsilon, victim set, and classification; they are linked, not merged.
 *   Epsilon's referent here is the standing absolutist arrangement itself,
 *   assessed by this reading's own lights: the reading knowingly prices the
 *   harm costs borne by targets of protected speech as the deliberate cost of
 *   categorical autonomy, so its own-lights epsilon is moderate-low even
 *   though those costs concentrate on identifiable, exitless people. The
 *   claimed type (tangled_rope) and the metrics are authored independently:
 *   the claim states my structural belief that a genuine anti-weaponization
 *   coordination function and an asymmetric, enforcement-dependent cost
 *   allocation share one structure; the metrics describe the arrangement's
 *   actual operation. The engine computes per-seat classifications from the
 *   structural data; divergence between the claim and the computed seats is
 *   the measurement this corpus exists to take.
 *
 * KEY AGENTS:
 *   - dissenting_speakers_and_whistleblowers: Primary beneficiary (moderate/constrained) — opposition and disclosure speech is the doctrine's core protected load
 *   - everyday_public_speakers: Diffuse beneficiary (powerless/constrained) — receive protection without bearing concentrated costs
 *   - targets_of_protected_harmful_speech: Primary payer (powerless/trapped) — bear uncompensated harm costs below the unprotected-category floor
 *   - constitutional_courts: Agenda setter (institutional/constrained) — administer and defend the threshold against recurring pressure
 *   - legislators_and_executives: Payer with agenda-setting ambition (institutional/constrained) — denied suppression tools, continuously propose carve-outs
 *   - platform_intermediaries: Dual-positioned beneficiary-administrator (institutional/arbitrage) — monetize the protected-speech environment and shape de facto boundaries
 *   - dignity_based_advocacy_movements: Excluded party (organized/constrained) — seek categorical protection their remedy class cannot receive
 *   - constitutional_theorists: Analytical observer (analytical/analytical) — map the settlement's logic comparatively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.34).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Near-Absolute Speech Protection with Extreme Harm Override Threshold (Absolutist Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional law/political philosophy/communication ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64').
narrative_ontology:cs_kernel_codification('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', fixed_text).
narrative_ontology:cs_authority_grounding('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', lineage).
narrative_ontology:cs_interpretation_layer_present('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64').
narrative_ontology:cs_reading_relation('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', foundational, suppression_is_graver_than_speech_harm).
narrative_ontology:cs_axiom_status(suppression_is_graver_than_speech_harm, holdable).
narrative_ontology:cs_axiom_grounding('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', suppression_is_graver_than_speech_harm, empirically_contingent).
narrative_ontology:cs_axiom('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', foundational, viewpoint_neutrality_admits_no_discretion).
narrative_ontology:cs_axiom_status(viewpoint_neutrality_admits_no_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', viewpoint_neutrality_admits_no_discretion, conventional).
narrative_ontology:cs_reference_frame('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', categorical_no_law_abridging_baseline).
narrative_ontology:cs_drift_state('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', contemporary_algorithmic_amplification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8a49e6e6-f0ac-494e-9fcd-c53a5b8c0c64', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, dissenting_speakers_and_whistleblowers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, everyday_public_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, platform_intermediaries).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_protected_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, legislators_and_executives).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, viewpoint_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, marketplace_of_ideas_hypothesis).
narrative_ontology:constraint_vindicates(speech_harm_boundary__absolutist_reading, chilling_effect_precaution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish unpopular political criticism, investigative disclosures, and protest speech. Their protection depends on the categorical line: any discretionary standard for limiting speech would be administered by the incumbents they criticize, and history shows opposition speech is the first sacrificed under flexible standards. Leaving the jurisdiction's speech order would mean emigrating away from the audience their speech needs, so exit is theoretical.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dissenting_speakers_and_whistleblowers, beneficiary,
    moderate, biographical, constrained, national).

% Speak in ordinary civic and commercial life under the same guarantee. They receive the protection diffusely and rarely notice it until they need it; they bear no concentrated cost from the arrangement beyond living alongside speech they may find offensive.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, everyday_public_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Absorb the harm costs of speech that falls below the narrow unprotected bars: coordinated harassment campaigns, demeaning demonstrations at funerals, false rumors that miss defamation thresholds, group-directed invective. No legal remedy attaches because the speech is protected by design; the harm follows them across venues and platforms and cannot be deleted. Their recourse is counterspeech, avoidance, or private blocking, none of which undoes dissemination.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_protected_harmful_speech, payer,
    powerless, biographical, trapped, national).

% Administer the line: strike overbroad restriction statutes, refuse suppression petitions, and absorb the political backlash each refusal generates. They carry the ongoing burden of holding the threshold against recurring pressure and periodically refine the category boundaries, such as redefining what counts as a true threat in the digital era, without moving the categorical core.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Respond to salient incidents, including flag burnings, funeral protests, and viral harassment waves, by proposing carve-outs and accountability statutes. Judicial review denies them these tools, so they bear the arrangement as permanently frustrated response capacity while continuously agitating to move the threshold; electoral incentives reward the agitation regardless of outcome.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislators_and_executives, payer,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, legislators_and_executives, agenda_setter).

% Operate global engagement-optimizing distribution systems under the shelter of the absolutist norm. They cite free-speech commitments to resist removal duties that comparable firms bear in dignity-based jurisdictions, externalizing harm costs onto targets while monetizing the attention the protected speech generates. Through moderation policy they now shape much of the practical content of speech governance, making them a second, unelected administrator of the boundary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, platform_intermediaries, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, platform_intermediaries, agenda_setter).

% Organize for categorical legal protection against personhood-denying speech: they litigate, draft legislation, and campaign after each visible harm episode. Their remedy class is barred from the settlement by design, so they enter the conversation only as petitioners whose requests the framework is built to refuse; the original settlement was designed without a seat for them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dignity_based_advocacy_movements, excluded,
    organized, generational, constrained, national).

% Map the settlement's logic from the Millian and Madisonian traditions, compare it against dignity- and balancing-based regimes abroad, and adjudicate internal-consistency disputes. They hold no material stake; their exit is the analytic one of reframing the question.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, platform_intermediaries).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes discretion from speech regulation entirely: by denying every faction, incumbent, and offended majority the tool of selective suppression, the categorical line prevents the censorship machinery from being redirected against opponents whenever power changes hands, and eliminates the anticipatory self-censorship that discretionary standards induce.
% TRANSFER_FUNCTION: Moves the cost of harmful-but-protected speech onto its specific targets, who bear harassment, invective, and falsehood without legal remedy, and converts that cost-bearing into immunity-from-suppression held by speakers at large; it also assigns courts a permanent defensive workload of refusing suppression demands.
% ABSENT_VOICES: Targets of protected harmful speech and the movements organizing on their behalf are present only as losing petitioners: the settlement's designers and repeat players were speakers, adjudicators, and latterly platforms, and the remedy class the targets seek, namely categorical protection against personhood-denying speech, was excluded from the option set before they arrived. Populations living under dignity-based settlements in peer democracies are likewise absent from a domestic conversation that treats the categorical line as the only coherent one.
% DISAPPEARANCE_RATIONALE: If the categorical line vanished overnight, every faction with a grievance would immediately seek carve-outs, including harassment statutes, offense codes, disinformation regimes, and protest buffer zones, and each new administration would inherit and redirect the machinery its predecessor built. Platforms would face immediate liability exposure and would over-remove under uncertainty; opposition speech, the doctrine's core protected load, would be the first casualty in each cycle. The speech environment would reorganize around whoever administers the new discretion.
% FOUNDING_PROBLEM: Recurrent capture of speech regulation by incumbent power: seditious libel prosecutions, wartime repression, and loyalty policing demonstrated that any discretionary standard for limiting speech is administered by the very actors opposition speech targets, and gets turned on them first.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the speaker-beneficiary set: legal historians of seditious libel and loyalty policing document the capture pattern; comparative constitutional scholars attest that the capture problem persists under every settlement, including dignity-based ones that manage it with different tools; and target-side advocates concede the capture risk even while disputing that absolutism is the only answer, so their testimony that the problem is live comes from the seat the arrangement costs the most.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).
:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.34, rising from 0.20 across the interval: the categorical line takes almost nothing from speakers, and the reading accepts target-borne harm as designed cost, but the costs are real, concentrated, and have grown as algorithmic amplification scaled harm delivery while the legal line stayed fixed, so extraction accumulates on the target side without any doctrinal change. Suppression (0.62) is authored as a raw structural property, unscaled by power or scope (the engine scales only extractiveness): it reflects structural foreclosure rather than violence, in that the arrangement compels officials to refuse relief, bars an entire remedy class by design, and forecloses rival settlements domestically, while functioning dignity- and balancing-based settlements abroad cap it below coercive-regime levels. Theater_ratio is low (0.18) because the protective function is overwhelmingly real; the slow rise tracks instrumental deployment of free-speech rhetoric by platforms and partisans who invoke the norm selectively. Accessibility_collapse is 0.35: understood or not, alternatives never collapsed, because peer democracies operate visibly different settlements. Resistance is high (0.68): every salient harm episode mobilizes organized pressure to move the threshold, defeated case-by-case rather than deterred. The measurement series share one eight-point grid (1969 to 2025 at eight-year steps) with all three metrics authored at every point; the dynamics are ratchet-with-spikes rather than smooth, since each moral panic briefly raises enforcement pressure that partially decays between episodes, and the series record the post-decay plateaus. A civic-identity fusion surrounds the norm (see omega civic_identity_fusion_of_speech_norm): for a substantial constituency the line is constitutive of national self-concept, which is why resistance to threshold movement takes betrayal-framing rather than cost-benefit framing.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergently from identical structural data. From the dissenting-speaker seat the arrangement is a protective subsidy with near-zero burden and existential benefit, computing rope-like or better. From the target seat the same structure operates as coercion-backed cost allocation with no exit: remedies foreclosed by design, harm following them across venues, computing snare-flavored. The judiciary seat experiences stewardship burden without collection, a roughly symmetric enforcement-mechanism position. The platform seat experiences subsidized arbitrage: sheltered from duties its foreign counterparts bear, monetizing the protected environment. The excluded advocacy seat experiences a closed door: full voice in petitioning, zero voice in design. The engine derives these from role, power, and exit data; the authored claim does not adjudicate among them. Coalition note: targets are individually powerless but episodically coordinate through advocacy organizations, and the organized-power seat in this story is that coalition; its remedy-class exclusion, not its weakness, is what keeps it from converting organization into threshold movement.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Dissenting and everyday speakers sit near the full-beneficiary end (d approximately 0.05 to 0.15): the constraint subsidizes them, they bear no concentrated cost, and their constrained exit does not convert them into payers. Targets of protected harmful speech sit near the full-target end (d approximately 0.85 to 0.95): they bear the allocated costs, and their trapped exit amplifies effective extraction because they cannot arbitrage away harm that follows them across venues. Constitutional courts derive near-symmetric (d approximately 0.45 to 0.55): permanent defensive workload borne, institutional function collected, no rent. Legislators and executives derive target-leaning (d approximately 0.60 to 0.70): the arrangement denies them tools they actively seek. Platforms derive strongly beneficiary-side (d approximately 0.10 to 0.15): listed beneficiaries whose arbitrage-grade exit pushes them further toward the subsidized end. Advocacy movements derive target-leaning despite organized power (d approximately 0.70 to 0.80): organized but remedy-less. No directionality_overrides are authored: the derivation differentiates the three institutional seats (courts, legislators, platforms) through role and exit data, whereas atom-keyed overrides would flatten precisely the intra-institutional divergence this story exists to measure. On the receipt surface: gain_flow names platform_intermediaries because the arrangement's operational surplus, namely liability shelter plus monetizable protected attention, demonstrably accrues to that seat, while speaker-class autonomy is a conferred condition spread across millions of seats rather than a collected gain, so 'diffuse' would understate the one seat capturing a concentrated flow. fixing_cost is prohibitive: replacing the categorical line means overturning entrenched doctrine, re-engineering platform governance, and absorbing factional conflict over every new discretion, against a benefit the kernel contest itself disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution is declared: the founding problem, capture of speech regulation by incumbent power, is live, attested by historians, comparativists, and from the costliest seat by target-side advocates themselves. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure rope hides the target-side cost allocation that grows with amplification technology; reading it as snare erases the genuine anti-weaponization function that even its targets rely on whenever they speak. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges, a consistent pair producing no zombie flag: the arrangement persists because its problem persists, not because anyone performs a dead mandate. The piton-direction risk sits elsewhere: if civic identity fusion outlasts the instrumental justification (omega civic_identity_fusion_of_speech_norm), the speaker seat could decay toward theatrical maintenance of a line nobody instrumentally needs; that omega records the trajectory and its detection signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_speech_harm_kernel,
    'This constraint is one reading (absolutist_reading) of the speech_harm_boundary kernel; what would the sibling readings change structurally?',
    'Compare the compiled sibling stories speech_harm_boundary__harm_balancing_reading and speech_harm_boundary__dignity_reading: the dignity reading expands the unprotected set to personhood-denying speech (changing the victim set and shrinking speaker immunity); the harm-balancing reading replaces the categorical line with case-by-case proportionality (making the threshold measurement-dependent).',
    'If a sibling reading were adopted as the operative settlement, this story''s victim set, epsilon, and per-seat classifications all shift; the disagreement is located in the harm override threshold and the unprotected-category boundary, not in the value of speech protection itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_speech_harm_kernel, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    natural_right_or_constructed_bargain,
    'Is the near-absolute line a discovered moral limit on state power or a constructed institutional bargain sustained only by active judicial maintenance?',
    'Comparative persistence analysis: track whether the line holds where doctrine is actively upheld and erodes where judicial personnel or doctrine shift; constructed bargains track their maintainers, discovered limits do not.',
    'A discovered-limit finding pushes the speaker-side classification toward mountain-like immunity; a constructed-bargain finding confirms the enforcement-dependent tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_or_constructed_bargain, conceptual, 'Natural-law framing versus constructed-bargain reality of the categorical line.').

omega_variable(
    target_harm_cost_magnitude,
    'How large are the harm costs borne by targets of protected speech, and can they be measured independently of the reading that prices them?',
    'Longitudinal cohort comparison of harassment and group-invective targets across absolutist and balancing/dignity jurisdictions, measuring psychological, economic, and participatory-withdrawal costs.',
    'Higher measured costs raise effective extraction for the target seat and strain the coordination half toward per-seat snare verdicts; lower measured costs support the deliberate-price framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_harm_cost_magnitude, empirical, 'Magnitude and measurability of target-borne harm costs under the categorical line.').

omega_variable(
    civic_identity_fusion_of_speech_norm,
    'Is speaker-side attachment to the absolutist line instrumentally held or identity-fused with national and civic self-concept?',
    'Survey and discourse analysis of responses to proposed reforms: instrumentally held norms respond to cost-benefit framing; identity-fused norms respond with betrayal framing that is immune to evidence.',
    'If fused, reform pressure misfires and the line persists past the point its instrumental justification decays, creating a piton-direction risk on the speaker seat; if instrumental, threshold movement remains negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_identity_fusion_of_speech_norm, conceptual, 'Identity-lock dynamics of free-speech culture around the categorical line.').

omega_variable(
    platform_administrator_status,
    'Are platform intermediaries incidental beneficiaries of a pre-existing constitutional norm, or have they become co-administrators whose moderation policy now determines the boundary''s practical content?',
    'Documentary comparison of moderation-policy evolution against judicial doctrine: if platform rules restrict speech the doctrine protects (or protect speech it restrains) and courts defer to the resulting equilibrium, co-administration is established.',
    'Co-administration relocates the agenda_setter seat, concentrates gain_flow further, and changes which enforcement events count as the constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_administrator_status, empirical, 'Whether platforms are beneficiaries or de facto second administrators of the boundary.').

omega_variable(
    internalized_vs_structural_target_acquiescence,
    'Is target-side under-use of available counterspeech and reporting channels due to structural futility or internalized acceptance of verbal harm as privately bearable?',
    'Post-remedy-availability trajectories: if targets pursue newly available remedies at high rates where jurisdictions introduce them, prior acquiescence was structural; if uptake stays low, acceptance is internalized.',
    'Internalized acceptance depresses measured resistance below true demand for change, biasing the resistance metric downward and masking latent coalition power among targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_target_acquiescence, empirical, 'Structural versus internalized mechanism behind target-side acquiescence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1969, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_harm_boundary__absolutist_reading, theater_ratio, 1969, 0.07).
narrative_ontology:measurement_basis(spee_tr_t1969, observed).
narrative_ontology:measurement(spee_tr_t1977, speech_harm_boundary__absolutist_reading, theater_ratio, 1977, 0.08).
narrative_ontology:measurement_basis(spee_tr_t1977, observed).
narrative_ontology:measurement(spee_tr_t1985, speech_harm_boundary__absolutist_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement_basis(spee_tr_t1985, observed).
narrative_ontology:measurement(spee_tr_t1993, speech_harm_boundary__absolutist_reading, theater_ratio, 1993, 0.11).
narrative_ontology:measurement_basis(spee_tr_t1993, observed).
narrative_ontology:measurement(spee_tr_t2001, speech_harm_boundary__absolutist_reading, theater_ratio, 2001, 0.13).
narrative_ontology:measurement_basis(spee_tr_t2001, observed).
narrative_ontology:measurement(spee_tr_t2009, speech_harm_boundary__absolutist_reading, theater_ratio, 2009, 0.15).
narrative_ontology:measurement_basis(spee_tr_t2009, observed).
narrative_ontology:measurement(spee_tr_t2017, speech_harm_boundary__absolutist_reading, theater_ratio, 2017, 0.17).
narrative_ontology:measurement_basis(spee_tr_t2017, observed).
narrative_ontology:measurement(spee_tr_t2025, speech_harm_boundary__absolutist_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(spee_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_harm_boundary__absolutist_reading, base_extractiveness, 1969, 0.2).
narrative_ontology:measurement_basis(spee_be_t1969, observed).
narrative_ontology:measurement(spee_be_t1977, speech_harm_boundary__absolutist_reading, base_extractiveness, 1977, 0.22).
narrative_ontology:measurement_basis(spee_be_t1977, observed).
narrative_ontology:measurement(spee_be_t1985, speech_harm_boundary__absolutist_reading, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement_basis(spee_be_t1985, observed).
narrative_ontology:measurement(spee_be_t1993, speech_harm_boundary__absolutist_reading, base_extractiveness, 1993, 0.27).
narrative_ontology:measurement_basis(spee_be_t1993, observed).
narrative_ontology:measurement(spee_be_t2001, speech_harm_boundary__absolutist_reading, base_extractiveness, 2001, 0.29).
narrative_ontology:measurement_basis(spee_be_t2001, observed).
narrative_ontology:measurement(spee_be_t2009, speech_harm_boundary__absolutist_reading, base_extractiveness, 2009, 0.31).
narrative_ontology:measurement_basis(spee_be_t2009, observed).
narrative_ontology:measurement(spee_be_t2017, speech_harm_boundary__absolutist_reading, base_extractiveness, 2017, 0.33).
narrative_ontology:measurement_basis(spee_be_t2017, observed).
narrative_ontology:measurement(spee_be_t2025, speech_harm_boundary__absolutist_reading, base_extractiveness, 2025, 0.34).
narrative_ontology:measurement_basis(spee_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_harm_boundary__absolutist_reading, suppression_requirement, 1969, 0.46).
narrative_ontology:measurement_basis(spee_su_t1969, observed).
narrative_ontology:measurement(spee_su_t1977, speech_harm_boundary__absolutist_reading, suppression_requirement, 1977, 0.49).
narrative_ontology:measurement_basis(spee_su_t1977, observed).
narrative_ontology:measurement(spee_su_t1985, speech_harm_boundary__absolutist_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement_basis(spee_su_t1985, observed).
narrative_ontology:measurement(spee_su_t1993, speech_harm_boundary__absolutist_reading, suppression_requirement, 1993, 0.54).
narrative_ontology:measurement_basis(spee_su_t1993, observed).
narrative_ontology:measurement(spee_su_t2001, speech_harm_boundary__absolutist_reading, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement_basis(spee_su_t2001, observed).
narrative_ontology:measurement(spee_su_t2009, speech_harm_boundary__absolutist_reading, suppression_requirement, 2009, 0.58).
narrative_ontology:measurement_basis(spee_su_t2009, observed).
narrative_ontology:measurement(spee_su_t2017, speech_harm_boundary__absolutist_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement_basis(spee_su_t2017, observed).
narrative_ontology:measurement(spee_su_t2025, speech_harm_boundary__absolutist_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(spee_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'free speech versus harm' decomposes under the epsilon-invariance principle into three structurally distinct settlements of the speech_harm_boundary kernel. This file instantiates the absolutist_reading; speech_harm_boundary__harm_balancing_reading and speech_harm_boundary__dignity_reading instantiate the siblings. Epsilon differs across the family because each reading prices the same standing arrangement by its own lights: the absolutist reading authors moderate-low epsilon (deliberately accepted target costs), the dignity reading authors high epsilon (unremedied personhood-denying harm), and the balancing reading authors epsilon conditional on demonstrated harm. Upstream/downstream: the absolutist reading sets the rhetorical and doctrinal baseline against which the siblings must argue, so it exerts structural pressure on both; the siblings exert reverse pressure at the boundary cases (harassment epidemics, dignity harms) that strain the absolutist threshold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
