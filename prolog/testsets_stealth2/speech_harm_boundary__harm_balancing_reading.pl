% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech-Harm Boundary — Harm-Balancing Reading (Presumptive Protection Yielding to Demonstrated Harm)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   In constitutional democracies following the post-war rights-instrument
 *   model, expressive liberty is presumptively protected; the state may
 *   restrict expression only upon demonstrating concrete harm, and any
 *   restriction must survive proportionality discipline (suitability,
 *   necessity, balance). This story instantiates the harm_balancing_reading
 *   of the speech_harm_boundary kernel as a clean, epsilon-invariant
 *   constraint: the standing arrangement under contest is the
 *   proportionality-balancing regime itself, assessed by this reading's own
 *   lights. Its epsilon is moderate (0.47): speakers bear real restriction
 *   costs when harm is demonstrated, and the unprotected categories have
 *   broadened well beyond the founding incitement/defamation core to
 *   encompass hate speech, group libel, and harassment — but the presumption
 *   still shields the bulk of discourse, and every restriction must run the
 *   demonstration gauntlet. CONSTRAINT FAMILY: the colloquial label
 *   'speech-harm boundary' decomposes into three readings of one kernel,
 *   authored as separate files. Sibling IDs are assumed to follow this file's
 *   naming pattern: speech_harm_boundary__absolutist_reading (near-absolute
 *   protection, extremely high override threshold, negligible epsilon) and
 *   speech_harm_boundary__dignity_reading (dignity-subordinate, categorical
 *   unprotectedness for personhood-denying speech, higher epsilon, no
 *   balancing check). Each sibling carries its own epsilon, beneficiaries,
 *   and victims; this file links them via network.affects_constraints and
 *   documents the decomposition in the dual-formulation note. Interval
 *   mapping: t=0 approximates the mid-1960s consolidation of modern
 *   proportionality review; t=60 approximates the mid-2020s
 *   digital-enforcement era.
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter (institutional/constrained) — administers the balancing framework and accumulates final adjudicative authority over expression conflicts
 *   - legislatures_and_regulators: co-administrator and beneficiary (institutional/mobile) — enacts hate-speech, group-libel, and harassment statutes within the framework's license
 *   - speakers_subject_to_restriction: primary target (moderate/constrained) — bears removal, injunction, penalty, and reputational costs when harm is demonstrated
 *   - marginal_dissenting_speakers: anticipatory target (powerless/trapped) — bears chilling costs without formal restriction
 *   - targets_of_harmful_speech: primary beneficiary (moderate/constrained) — receives recourse against harassment, group defamation, and incitement
 *   - advocacy_organizations: organized beneficiary (organized/mobile) — litigates test cases and supplies the harm evidence the demonstration requirement consumes
 *   - general_public_speakers: diffuse beneficiary (powerless/mobile) — enjoys the presumptive-protection half of the framework
 *   - platforms_and_publishers: dual-positioned payer/beneficiary (powerful/arbitrage) — bears compliance and over-removal costs while gaining safe-harbor clarity
 *   - civil_liberties_organizations: engaged observer (organized/mobile) — litigates against overbreadth and shapes which applications survive review
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.47).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.62).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech-Harm Boundary — Harm-Balancing Reading (Presumptive Protection Yielding to Demonstrated Harm)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3').
narrative_ontology:cs_kernel_codification('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', formalized).
narrative_ontology:cs_authority_grounding('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', lineage).
narrative_ontology:cs_interpretation_layer_present('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3').
narrative_ontology:cs_reading_relation('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', foundational, presumptive_protection_yields_to_demonstrated_harm).
narrative_ontology:cs_axiom_status(presumptive_protection_yields_to_demonstrated_harm, holdable).
narrative_ontology:cs_axiom_grounding('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', presumptive_protection_yields_to_demonstrated_harm, instrumental).
narrative_ontology:cs_axiom('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', secondary, proportionality_discipline_legitimates_restriction).
narrative_ontology:cs_axiom_status(proportionality_discipline_legitimates_restriction, holdable).
narrative_ontology:cs_axiom_grounding('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', proportionality_discipline_legitimates_restriction, conventional).
narrative_ontology:cs_reference_frame('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', presumptive_liberty_proportionate_limitation).
narrative_ontology:cs_drift_state('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', digital_platform_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c3b4f4d-24e6-4efa-91b0-fe1a24ef6ee3', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, advocacy_organizations).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, general_public_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, constitutional_courts).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, marginal_dissenting_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, platforms_and_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, platforms_and_publishers).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_review_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, prescribed_by_law_necessity_limitation_clauses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review speech restrictions against the demonstration-of-harm and proportionality requirements, refine the test's stages through precedent, and in doing so accumulate final adjudicative authority over politically charged expression conflicts. Revising the framework toward a sibling reading would spend accumulated doctrinal capital and provoke separation-of-powers conflict; incremental adjustment is the realistic path, so they stay inside the structure they administer while collecting jurisdiction from its operation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, constitutional_courts, beneficiary).

% Enact hate-speech, group-libel, and harassment statutes within the license the framework grants, gaining legitimate restriction powers that stricter readings would deny them. They can amend statutory categories each session and shift enforcement emphasis with electoral cycles, though constitutional review bounds what survives.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators, agenda_setter).

% Individuals and groups subjected to harassment, group defamation, and incitement. They obtain removal orders, injunctions, and damages without needing to win a categorical redefinition of speech protection. Private tort remedies alone are slow and weak against distributed online harm, which makes the constitutional route their effective path and locks in their reliance on the framework.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech, beneficiary,
    moderate, biographical, constrained, national).

% Organizations representing targeted groups litigate test cases, draft proposed statutory categories, and supply the harm evidence the demonstration requirement consumes. They can shift between litigation, legislative lobbying, and platform-pressure strategies as venues open and close, and their continued relevance depends on the framework remaining the operative adjudication route.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, advocacy_organizations, beneficiary,
    organized, generational, mobile, continental).

% The presumptive-protection half of the framework shields everyday expression from prior restraint and licensing regimes. The benefit is diffuse and mostly invisible to its holders unless threatened; speakers retain access to countless channels the framework does not touch.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, general_public_speakers, beneficiary,
    powerless, generational, mobile, national).

% Speakers whose expression is found, after process, to constitute actionable harm. They bear removal, injunction, penalty, and reputational costs. Relocating their expressive activity out of the doctrine's reach is impractical where enforcement follows online distribution across borders, and the demonstration burden operates in practice as a cost they must litigate against.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction, payer,
    moderate, biographical, constrained, national).

% Dissidents, satirists, and minority critics whose speech sits nearest the harm line. They self-censor anticipatorily because a harm finding against them is both plausible and ruinous, and no alternative channel reliably escapes classification risk. For activist identities fused with speaking, exit into silence is not a real option, so they bear the constraint's costs continuously without any formal finding ever issuing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, marginal_dissenting_speakers, payer,
    powerless, biographical, trapped, national).

% Host and distribute the expression the framework governs. They bear takedown-compliance costs, over-removal risk, and liability exposure, while gaining clear safe-harbor boundaries and commercial demand for trust-and-safety services. They arbitrage across jurisdictions by geofencing content and tailoring policy per market, which blunts the extraction they would otherwise absorb at full weight.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, platforms_and_publishers, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, platforms_and_publishers, beneficiary).

% Litigate against overbroad categories, publish critiques of balancing's malleability, and defend individual speakers pro bono. They neither run the framework nor pay its costs, but their challenges determine which applications survive review, and they can redirect effort between courts, legislatures, and public argument as opportunities shift.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, civil_liberties_organizations, observer,
    organized, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, targets_of_harmful_speech).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, administrable standard for resolving conflicts between expressive liberty and harm prevention: which speech acts may be restricted, on what evidentiary showing, and in what proportion — so that legislatures, courts, platforms, and speakers can coordinate expectations without collapsing into either blanket prohibition or blanket immunity.
% TRANSFER_FUNCTION: Moves unrestricted communicative space from speakers whose expression is found harmful to the targets of that speech (as protection), and moves enforcement discretion to legislatures and final adjudicative authority to constitutional courts; speakers pay in removed expression, platforms in compliance costs.
% ABSENT_VOICES: Speakers governed by exported versions of the framework in jurisdictions where harm findings suppress dissent — political dissidents, religious minorities, marginalized critics — are present only as case files, never as authors of the standard. Future speakers whose interests are discounted in present balancing have no seat at all. Radical dissenters whose expression is precisely what balancing regimes tend to classify as harmful appear as respondents, not as participants in designing the demonstration threshold.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, speech regulation would reorganize around either categorical lines (the sibling readings) or unreviewed state discretion; thousands of statutes — hate-speech laws, harassment injunctions, platform takedown mandates — would lose their doctrinal foundation, pending litigation would surge, and platform compliance architecture built to the framework's specifications would require wholesale redesign. Arrangements across the adjudicative, legislative, and platform layers depend on it.
% FOUNDING_PROBLEM: The post-war problem: how to reconcile democratic commitment to free expression with the demonstrated capacity of mass propaganda, incitement, and group defamation to destroy persons and democracies — drawing a principled line between protected expression and punishable harm without handing government a censorship instrument. The human-rights limitation clauses (law-prescribed, necessary, proportionate) were the negotiated answer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the UN Human Rights Committee's General Comment 34 and the post-war treaty record attest the incitement and propaganda problem the limitation clauses answer; comparative constitutional scholarship documents the Weimar-era failure that motivated structured balancing; civil-liberties organizations — adversaries of specific applications, not beneficiaries of the arrangement — attest the underlying problem remains live. No external corroboration exists for the claim that today's broadened categories (hate speech, group libel, harassment) remain necessary to the founding problem; that extension rests on the benefiting coalition's own testimony, which is itself signal.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.47 at interval end) because restriction costs on speakers are real and the unprotected categories have broadened, but the presumption of protection still governs most expression and every restriction must pass the demonstration requirement. Suppression (0.62) reflects the enforcement machinery's growth: from occasional criminal prosecution at t=0 to systematic notice-and-takedown mandates, network-level injunctions, and platform liability at t=60 — coercive, but leaving counter-speech channels open. Theater (0.30) is below the substitution threshold: balancing remains mostly substantive, though the rising series marks growing formulaicism in settled categories. Accessibility collapse is low-moderate (0.40): speakers retain reframing, counter-speech, anonymized channels, jurisdictional arbitrage, and litigation as alternatives, so understanding the constraint does not collapse the option space. Resistance (0.55) is sustained: civil-liberties litigation, academic critique of balancing's malleability, and the living counter-model of absolutist jurisdictions continuously contest expansions. The measurement series run on one shared time grid (every tracked metric authored at every point 0-60); the trajectories are monotonic ratchets, with episodic crisis-driven spikes (terror attacks, harassment scandals) superimposed but not sampled as separate phases. Coalition note: the powerless marginal-speaker seat is not without aggregate leverage — dissenters, civil-liberties organizations, and platforms resisting overbreadth form the standing coalition that has kept extraction from ratcheting past moderate, and is the main brake on further drift. Claim/metric independence: tangled_rope is claimed from structure (genuine coordination function + asymmetric extraction + active enforcement); the metrics are authored as descriptively true and are not tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from the same doctrine. From the payer seats (restricted speakers, marginal dissenters), the framework operates as enforced extraction: their expression is the object regulated, the demonstration burden falls on them in practice, and for identity-fused activist speakers exit into silence is not a real option, which pushes their computed classification toward the extractive end. From the beneficiary seats (harm targets, advocacy organizations), the same structure operates as hard-won protection purchased at the price of case-by-case proof. From the agenda-setter seat (constitutional courts), it operates as principled adjudication the judiciary refined over six decades — a coordination achievement. The engine computes this divergence from the structural data (power, exit, directionality); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Targets of harmful speech and advocacy organizations sit near the beneficiary end (low d): the framework subsidizes them with recourse they could not otherwise obtain, and their constrained exit (weak private remedies) locks in the subsidy. General public speakers are diffusely subsidized by the presumption (low-moderate d). Constitutional courts are the notable agenda-setter-with-low-d shape: they administer the constraint AND collect adjudicative authority from its operation, so their derived directionality is beneficiary-flavored despite running the machinery. Legislatures gain discretionary power (low-moderate d) but bear electoral and backlash costs that keep them from the pure-beneficiary end. Restricted speakers sit near the target end (high d): they bear the transfer directly with constrained exit. Marginal dissenting speakers sit nearest the full-target end: trapped exit plus anticipatory costs they bear without any formal finding. Platforms are genuinely mixed — victim-array membership pushes d upward while arbitrage-grade exit (geofencing, per-market policy) pulls it back toward symmetry; the structural data lets the engine resolve this without an override. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct relationships, and overrides would be redundant.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabels. Reading the framework as pure rope ('everyone's interests are balanced') erases the asymmetric extraction: speakers pay, categories have broadened beyond the founding core, and the demonstration requirement is satisfied in practice by harm narratives rather than causal proof. Reading it as pure snare ('harm is the censorship license') erases the genuine coordination function: an administrable, reviewable standard replaced ad hoc state discretion, and the presumption-plus-demonstration structure is precisely what distinguishes this regime from licensing systems. Tangled rope holds both truths. On mandatrophy proper: the founding problem (drawing a principled line against mass incitement and defamation without handing government a censorship instrument) is LIVE — mass propagation of incitement and harassment still causes demonstrable harm — so no mandatrophy is resolved; founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no mismatch flag. The monitoring signal is the theater_ratio trajectory (0.12 to 0.30): ritualization in settled categories is the leading indicator that would convert this into a piton if the doctrine's substance ever atrophies behind its performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the harm_balancing_reading of the speech_harm_boundary kernel — what structurally changes if a sibling reading governs the same boundary instead?',
    'Comparative-jurisdictional analysis: observe unprotected-category breadth, epsilon, and speaker-cost incidence across absolutist-governed systems (near-absolute protection, extremely high harm-override threshold), harm-balancing systems (this reading), and dignity-governed systems (personhood-denying speech categorically unprotected).',
    'Under the absolutist sibling, unprotected categories shrink toward incitement-only and speaker restriction costs fall toward negligible (epsilon drops sharply; the victim set thins). Under the dignity sibling, personhood-denying speech becomes categorically unprotected without case-by-case harm demonstration, raising epsilon and removing the proportionality check that currently disciplines restriction. This file''s epsilon (0.47) and victim set are valid only for the harm-balancing instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the speech-harm-boundary kernel; sibling readings instantiate different constraints with different epsilon, category shapes, and victim sets.').

omega_variable(
    demonstrated_harm_evidentiary_threshold,
    'What actually counts as ''demonstrated'' harm — causal proof linking expression to measurable injury, or intuitively plausible harm narratives?',
    'Longitudinal causal studies linking specific expression classes to measurable psychological, economic, and social harm, cross-checked against how courts actually treat evidence at the demonstration stage.',
    'A strict evidentiary threshold shrinks the unprotected categories toward the absolutist sibling''s profile; a loose threshold expands them toward the dignity sibling''s categorical breadth, moving epsilon in each direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_evidentiary_threshold, empirical, 'Evidentiary content of the harm-demonstration requirement that triggers the override.').

omega_variable(
    anticipatory_chill_magnitude,
    'How much self-censorship does the demonstration-and-balancing machinery induce among speakers who are never formally restricted?',
    'Survey and natural-experiment data on expressive behavior under varying enforcement intensity; comparison of speech volumes before and after high-profile harm findings against similar speakers.',
    'A large anticipatory chill raises effective extraction on the marginal-speaker seat well above the formally measured restriction rate, shifting that seat''s computed classification toward the extractive end independently of formal enforcement statistics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticipatory_chill_magnitude, empirical, 'Magnitude of the chilling effect relative to formal restriction.').

omega_variable(
    selective_enforcement_valence_asymmetry,
    'Is the harm-demonstration machinery applied symmetrically across political valence and speaker identity, or is it captured by dominant groups to silence criticism?',
    'Audit of enforcement patterns (complaints filed, findings made, penalties imposed) disaggregated by speaker viewpoint, group membership, and target identity across jurisdictions.',
    'Systematic asymmetry would convert the tangled-rope structure into a snare for the targeted classes: the coordination story would remain as cover while extraction concentrates on identifiable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_valence_asymmetry, empirical, 'Whether enforcement of the harm override is viewpoint-symmetric or captured.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (penalties, mandated removal, injunctions) or internalized (speakers pre-conforming to anticipated harm findings)?',
    'Post-liberalization speech trajectories: where restrictions are struck down or repealed, observe whether previously suppressed expression rebounds; persistent absence indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and persists after formal reform — speakers carry the boundary with them after the enforcement machinery is withdrawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized component of the suppression scalar.').

omega_variable(
    balancing_substance_vs_ritual,
    'Is the proportionality analysis a genuine weighing that could come out either way, or a formula whose outcomes are predictable from category labels alone?',
    'Outcome-prediction studies of decided balancing cases: if outcomes are predictable from the speech category and applicant identity without the staged analysis, the balancing is substantially ritual.',
    'High ritual share raises theater_ratio further and signals piton-direction drift in mature doctrine: the test persists as performance while real decisions are made by category habit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_substance_vs_ritual, empirical, 'Substantive versus performative share of the proportionality exercise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(spee_tr_t30, observed).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__harm_balancing_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(spee_tr_t50, observed).
narrative_ontology:measurement(spee_tr_t60, speech_harm_boundary__harm_balancing_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(spee_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(spee_be_t30, observed).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(spee_be_t50, observed).
narrative_ontology:measurement(spee_be_t60, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement_basis(spee_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(spee_su_t30, observed).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(spee_su_t50, observed).
narrative_ontology:measurement(spee_su_t60, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(spee_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'speech-harm boundary' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — measuring the boundary by the harm-balancing reading's observables yields moderate epsilon (~0.47) with speakers bearing demonstrated-harm restriction costs, while the absolutist reading's observables yield negligible epsilon and the dignity reading's yield higher epsilon with categorical victim sets. These are not one constraint viewed from angles; they are different constraints with different failure modes, and each is authored as a separate file. Structural edges: this reading COEXISTS with the absolutist reading (different jurisdictions and judicial coalitions hold each simultaneously; neither logically eliminates the other as a live position) and INFLUENCES the dignity reading (the proportionality machinery supplies the procedural vehicle through which dignity claims are adjudicated, and the broadened balanced categories absorb part of the dignity reading's subject matter, changing its operating environment without foreclosing it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
