% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection Boundary (High Speaker Autonomy, Narrow Unprotected Category)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary protects speech
 *   near-absolutely, permitting legal restriction only for narrow categories:
 *   incitement to imminent violence, true threats (threats of lawless action
 *   directed at a specific person or group), defamation (false statements of
 *   fact with high evidentiary standards), and obscenity. The reading
 *   prioritizes speaker autonomy and rejects proportionality-based harm
 *   balancing, asserting that the costs of allowing speech restriction—via
 *   authorities weaponizing harm standards—outweigh the costs of leaving
 *   targets without legal remedy for many forms of harmful speech. This is
 *   ONE reading of the contested speech-harm boundary kernel. Sibling
 *   readings include the dignity reading (personhood-denying speech is
 *   categorically unprotected) and the harm-balancing reading (speech
 *   protection is presumptive but yields to demonstrated harm with
 *   proportionality). The absolutist reading is claimed as a Rope (genuinely
 *   solves the coordination problem of preventing speech-restriction
 *   weaponization by authorities) but authored with high extractiveness
 *   metrics because it demonstrably allocates substantial costs to targets of
 *   harassment and defamation without legal remedy. The claim/metric gap is
 *   deliberate and structural: from the absolutist seat, the constraint is
 *   pure coordination (protection against government overreach); from the
 *   target seat, it is asymmetric extraction (you bear the harm, I gain the
 *   protection). The engine will compute different types per seat from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - speakers_political_unpopular: beneficiaries of absolutist protection (moderate power, mobile exit)
 *   - speakers_minority_viewpoint: beneficiaries particularly vulnerable to weaponized speech restrictions (powerless, constrained exit)
 *   - targets_hate_speech_campaigns: primary victims (moderate power, constrained exit)
 *   - targets_doxxing_coordinated_harassment: primary victims with no legal recourse via speech suppression (powerless, trapped exit)
 *   - targets_defamatory_falsehood: victims with expensive/slow legal remedy (moderate power, constrained exit)
 *   - courts_adjudicators: agenda-setters interpreting and enforcing the boundary (institutional power)
 *   - platforms_intermediaries: excluded from the constitutional rule but influenced by its framing (institutional power, mobile exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.12).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection Boundary (High Speaker Autonomy, Narrow Unprotected Category)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '492e7430-76db-46a5-86b7-fa20e98fd27e').
narrative_ontology:cs_kernel_codification('492e7430-76db-46a5-86b7-fa20e98fd27e', formalized).
narrative_ontology:cs_authority_grounding('492e7430-76db-46a5-86b7-fa20e98fd27e', lineage).
narrative_ontology:cs_interpretation_layer_present('492e7430-76db-46a5-86b7-fa20e98fd27e').
narrative_ontology:cs_reading_relation('492e7430-76db-46a5-86b7-fa20e98fd27e', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('492e7430-76db-46a5-86b7-fa20e98fd27e', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('492e7430-76db-46a5-86b7-fa20e98fd27e', foundational, speaker_autonomy_foundational).
narrative_ontology:cs_axiom_status(speaker_autonomy_foundational, holdable).
narrative_ontology:cs_axiom_grounding('492e7430-76db-46a5-86b7-fa20e98fd27e', speaker_autonomy_foundational, deontological).
narrative_ontology:cs_axiom('492e7430-76db-46a5-86b7-fa20e98fd27e', foundational, marketplace_of_ideas_sufficient_remedy).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('492e7430-76db-46a5-86b7-fa20e98fd27e', marketplace_of_ideas_sufficient_remedy, instrumental).
narrative_ontology:cs_reference_frame('492e7430-76db-46a5-86b7-fa20e98fd27e', speech_autonomy_foundational).
narrative_ontology:cs_drift_state('492e7430-76db-46a5-86b7-fa20e98fd27e', contemporary_internet_coordination_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('492e7430-76db-46a5-86b7-fa20e98fd27e', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_political_unpopular).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_minority_viewpoint).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_institutional_dissent).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_hate_speech_campaigns).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_doxxing_coordinated_harassment).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_defamatory_falsehood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Express views that mainstream opinion opposes or finds offensive (anti-abortion rhetoric, immigration skepticism, heterodox economic claims, religious criticism). Under absolutist protection, their speech is protected even when deeply offensive, as long as it does not fall within the narrow unprotected categories (incitement to imminent violence, true threats, defamation). They benefit from a rule that shields them from legal liability for offense or discomfort caused.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_political_unpopular, beneficiary,
    moderate, biographical, mobile, national).

% Articulate views representing marginalized or non-hegemonic positions (dissidents, religious minorities, dissenters from institutional orthodoxy). The absolutist boundary protects their speech from suppression via civil or criminal liability even when it offends powerful actors or established doctrine. Exit from speech itself is not meaningful; their constraint is access to protected expression.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_minority_viewpoint, beneficiary,
    powerless, biographical, constrained, national).

% Institutional actors (universities, civil liberties organizations, journalism guilds, research communities) whose legitimacy depends on freedom to publish, critique, and challenge prevailing norms. They benefit from a legal boundary that does not require weighing harm against speech value—it maximizes institutional autonomy to set their own standards.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_institutional_dissent, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, speakers_institutional_dissent, observer).

% Experience organized campaigns of abusive speech based on identity (race, religion, gender, sexual orientation, disability). Under absolutist protection, coordinated harassment and dehumanizing rhetoric are protected speech if they do not meet the narrow unprotected thresholds (true threat, incitement to imminent violence). They bear the costs of the harassment directly—emotional harm, social isolation, economic consequence (lost opportunities, employment harassment)—without legal remedy via speech regulation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_hate_speech_campaigns, payer,
    moderate, biographical, constrained, national).

% Subjected to coordinated campaigns of doxing (personal information disclosure), swatting, harassment, and threats organized across internet platforms. The absolutist boundary protects the organizing speech itself—the identifying, locating, and targeting information—as long as it is not technically a true threat (a threat of imminent lawless action). They face sustained harassment with limited legal recourse via speech suppression; their exit is withdrawal from public participation or relocation.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_doxxing_coordinated_harassment, payer,
    powerless, biographical, trapped, global).

% Are subject to published false statements of fact that damage reputation or livelihood. The absolutist reading narrows defamation liability to false statements of fact proven with high evidentiary standards (actual malice for public figures; negligence for private figures). They bear reputational and economic harm even from provably false speech if the defamer's intent does not meet the legal threshold; legal remedy is available but expensive and slow relative to the speed of speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_defamatory_falsehood, payer,
    moderate, biographical, constrained, national).

% Digital platforms (social media, content hosts) are not bound by the absolutist constitutional reading—they are private actors and can set their own content standards. But the absolutist boundary sets expectations: platforms that adopt stronger speech protection (mirroring the constitutional rule) gain legitimacy with free-speech-defending constituencies and shield themselves from legal liability. Platforms excluded from the constitutional rule but influenced by its framing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, platforms_intermediaries, observer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, platforms_intermediaries, excluded).

% The legal reading sets the constitutional boundary and thus the constraint on what legislative action is permitted. Under the absolutist reading, legislatures cannot enact speech restrictions beyond the narrow unprotected categories without constitutional amendment or court re-reading. They administer the rule and defend it in courts, but cannot easily update it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, democratic_institutions_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Interpret and enforce the absolutist boundary via First Amendment jurisprudence. They decide which speech falls within the narrow unprotected categories and apply the rule to disputes. Their interpretation sets the operative constraint; their rulings determine effective protection.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_adjudicators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, courts_adjudicators, observer).

% Critique and defend the absolutist reading from multiple angles (deontological autonomy claims, empirical marketplace-of-ideas hypotheses, dignity-based alternatives). They produce the conceptual landscape in which the constraint is justified and contested. Analytical seat; their work does not directly benefit from or bear costs of the rule.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, academic_philosophy_legal_theory, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, speakers_institutional_dissent).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate legal boundary for speech protection that speakers can rely on without fear of civil or criminal liability for offense, discomfort, or contestable harm. Coordinates expectation that free expression is a foundational right by establishing a rule that does not require case-by-case harm balancing.
% TRANSFER_FUNCTION: Allocates the costs of harmful speech (emotional harm, reputation damage, harassment) from speakers to targets. Speakers gain protection from liability; targets bear the costs of speech that harms them but does not meet the narrow unprotected thresholds (incitement to imminent violence, true threats, defamation with actual malice). The rule moves the burden of response away from legal action and onto targets (social counteraction, platform moderation, personal coping) or onto alternative remedies (defamation suits with high burden of proof, true threat prosecution with imminent-action standard).
% ABSENT_VOICES: Targets of hate speech campaigns and coordinated harassment are structurally excluded from the primary legislative and philosophical debates that shape the absolutist boundary—their voices enter mainly as the objects of concern in harm-balancing critiques, not as authoring parties to the rule itself. Speakers from marginalized communities who also experience being targeted by coordinated campaigns occupy an ambiguous position: they benefit as speakers but bear costs as targets, a conflict unresolved by the absolutist frame.
% DISAPPEARANCE_RATIONALE: If the absolutist boundary vanished overnight and speech law shifted to proportionality-based harm balancing, the landscape would reorganize: speakers would face civil or criminal liability for speech that caused demonstrable harm (even without incitement or true threats), defamation standards would lower, harassment campaigns might be regulable as coordinated speech torts, and institutional actors (universities, media, research institutions) would need to navigate broader legal exposure for published work. The constraint actively structures what speakers can do; removing it would shift liability.
% FOUNDING_PROBLEM: Restrictions on speech can be deployed by power-holders to suppress dissent and entrench orthodoxy; a bright-line protection of expression (rather than proportionality balancing) prevents authorities from weaponizing harm standards to silence opposition.
% FOUNDING_PROBLEM_CORROBORATION: Free-speech advocates and First Amendment scholars attest that the founding problem is live: hate-speech laws in other democracies have been used to suppress minority speech and political opposition, and discretionary harm-balancing invites abuse. Harm-balancing and dignity scholars from outside the absolutist camp attest that the founding problem is overstated and that the absolutist solution creates its own harms (leaving targets without remedy). The empirical and normative contest is ongoing; corroboration comes from both sides' pointing to different historical cases and different theories of how speech restrictions function.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the absolutist boundary demonstrably shifts costs to targets: hate-speech targets, doxxed individuals, and defamation victims bear the social and economic harm of speech that is legally protected. The harm override threshold is set so high (incitement to imminent violence is narrow; true threats require specific threat of lawless action; defamation requires actual malice for public figures) that most harmful speech is protected. Suppression is extremely low (0.12) because the constraint does not require active enforcement—it operates via legal protection of speaker freedom, not coercion of speakers or targets. Theater is minimal (0.08) because the rule's operation is direct: speech either falls within the narrow unprotected categories or it is protected. The function is genuine but asymmetric: the coordination benefit (protection against speech-restriction weaponization) accrues to speakers and institutional defenders; the extraction (shifted harm costs) falls on targets. Accessibility_collapse is low (0.22) because alternatives to the absolutist boundary exist (harm balancing, dignity-based approaches, proportionality) and are actively advocated; the boundary is contested, not inevitable. Resistance is high (0.71) because targets, harm-balancing advocates, dignity scholars, and some platform operators actively contest the absolutist reading and push for narrower speaker protection or explicit harm-mitigation clauses. The time series show modest extraction growth (0.58 to 0.68 over 40 intervals) driven by internet-scale coordination enabling larger harassment campaigns relative to early constraint formation; the boundary itself does not tighten, but the harm it permits has accelerated as communication technology enabled mass targeting.
 *
 * PERSPECTIVAL GAP:
 *   From the absolutist speaker seat: this is pure coordination—a bright-line rule that protects dissent and prevents authorities from weaponizing harm standards. The extraction narrative is misplaced because the 'extraction' is a side effect of preventing something worse (speech-restriction abuse by power-holders). From the target seat: this is asymmetric extraction—I am denied legal remedy for demonstrable harm because of a rule designed to protect speakers from a different harm (government overreach). The target does not experience the constraint as protecting them against government weaponization of speech restrictions; they experience it as abandonment. Courts and legislatures occupy a middle position: they must administer a rule (the absolutist boundary) that they did not choose, and that constrains their options in response to target harm (they cannot easily enact broader speech restrictions without constitutional amendment). The directionality computation will reflect this: a powerless target will compute d near 1.0 (full extraction); an institutional speaker will compute d near 0.0 (full beneficiary); a court will compute d near 0.5 or constrained upward (they are agenda-setters but constrained by the rule).
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers are beneficiaries (d near 0.0): they gain protection from liability and enforcement against them is minimal. Targets are victims (d near 1.0): they bear harm costs that speakers escape via the narrow unprotected categories. The absolutist frame itself treats this as asymmetric by design—the point is to protect speakers from government enforcement, which necessarily leaves targets without legal speech-suppression remedies. Institutional speakers (universities, journalism, civil liberties organizations) occupy a hybrid position (secondary observer role): they benefit as speakers but also represent some targets (journalists covering harassment targets, universities hosting speakers whose speech harms students). Courses and adjudicators are agenda-setters (d near 0.5): they implement the boundary and could in principle shift it, but the absolutist reading constrains their choices—they are not neutral administrators but rather enforcers of a specific protection regime. Platforms are excluded (not seated as beneficiaries or victims in the constitutional rule, but influenced by it): private actors who could set a different standard but whose legitimacy narrative is often shaped by absolutist free-speech rhetoric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—that speech-restriction standards can be weaponized by authorities—is live and remains a legitimate concern. However, the absolutist reading's response (protect speech near-absolutely, leaving targets without remedy) is contested on two grounds: (1) empirically, whether the harms avoided (government speech suppression) outweigh the harms permitted (target harassment, defamation), and (2) normatively, whether there are middle-ground approaches (proportionality balancing, narrow hate-speech carve-outs) that reduce both kinds of harms. The mandatrophy risk is moderate: the founding problem persists, but the remedy's necessity is increasingly questioned as internet-scale coordinated harassment has become salient. The constraint has not lost its function (it still prevents government speech restrictions), but targets and harm-balancing advocates increasingly deny that this function justifies the cost. This is not yet classical mandatrophy (the founding problem is not dead), but it is trending toward it—a constraint solving a real problem while creating a new one that the original problem-solvers did not anticipate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_empirics,
    'What is the empirical relationship between speech-restriction breadth and government weaponization of speech law? Does the absolutist high threshold actually prevent abuse, or do high-threshold abuse risks emerge for other reasons?',
    'Comparative analysis of jurisdictions with absolutist vs. balanced speech regimes, examining abuse patterns over time and the actual drivers of speech-law weaponization (political pressure, institutional capture, normative drift vs. legal breadth). Empirical study of whether speech-restriction breadth correlates with speech-law abuse.',
    'If empirical analysis shows that high thresholds do not meaningfully reduce abuse (abuse occurs via prosecutorial discretion, legislative drift, or norm shifts regardless of formal breadth), the justification for the absolutist boundary weakens, and a proportionality-balancing approach might solve the coordination problem equally well while reducing target harm. If high thresholds are empirically protective against abuse, the extraction becomes a justified side effect of preventing something worse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_threshold_empirics, empirical, 'Whether the absolutist threshold achieves its stated function of preventing speech-restriction abuse.').

omega_variable(
    coordination_extraction_boundary_speech,
    'Is the coordination function (protection against government speech-restriction abuse) structurally inseparable from the extraction function (leaving targets without legal remedy for harmful speech)?',
    'Thought experiment and jurisprudential analysis: does a narrower absolutism (incitement + true threats only, but broader defamation/harassment recovery) solve the abuse problem equally well while reducing target harm? Empirical comparison of jurisdictions that protect core speech (political, religious) near-absolutely but allow remedy for targeted harassment.',
    'If the functions are separable (some coordination without the full extraction), the absolutist reading is over-exclusive—it protects more speech than necessary to solve the abuse problem. If they are structurally inseparable (protecting core speech requires the high threshold that also protects harassment), the extraction is a necessary cost, not a side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_speech, conceptual, 'Whether the harm-protection and abuse-prevention functions can be decoupled.').

omega_variable(
    target_agency_and_response,
    'Do targets of speech harm experience the absolutist boundary as constraining their agency (they cannot legally respond) or as empowering them (they develop social counteraction, platform moderation, and resilience)?',
    'Qualitative research on target experiences of speech harm and response strategies; analysis of whether target-initiated platform moderation, social accountability, and counter-speech create genuine alternative remedies or merely displace harm (transfer it to marginalized speakers via platform enforcement). Post-exit trajectory analysis: do targets who exit speech engagement permanently show different harm trajectories than those who remain engaged in counter-speech?',
    'If targets develop effective alternative remedies (platform moderation, social accountability, counter-speech), the extraction is lower than the metric suggests (targets do have agency). If exit is the primary remedy (targets withdraw from public engagement), the extraction is higher (deprivation of voice is the cost, not just emotional harm). This affects the piton vs. snare classification: if targets are genuinely powerless, the constraint trends snare; if they develop alternative remedies, it remains rope-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_agency_and_response, empirical, 'Whether targets possess effective agency for response beyond legal remedy.').

omega_variable(
    internet_coordination_scale_shift,
    'Does internet-scale coordination of harassment (organized campaigns across platforms, algorithmic amplification, distributed attacking) change the structural nature of the constraint by enabling harms previously impossible at pre-internet scale?',
    'Historical analysis comparing harm magnitudes and persistence pre- and post-internet scaling; empirical study of whether targets experience coordinated harassment as categorically different from individual speech acts (systemic vs. episodic harm). Comparison of how the absolutist boundary operated as a live doctrine in low-coordination contexts vs. high-coordination contexts.',
    'If internet coordination is categorically new (enables harms not anticipated when the absolutist boundary was formulated), the constraint''s adequacy is questioned—what was an acceptable trade-off (let individual harmful speakers go free to prevent speech-restriction abuse) may become unacceptable when harms are coordinated and persistent. The constraint may drift from rope (genuine coordination benefit) to snare (extraction via coordination technology the rule did not anticipate). This feeds mandatrophy analysis: the founding problem (government speech-restriction abuse) was real in the coordination contexts of the constraint''s formulation; whether it remains the dominant harm is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internet_coordination_scale_shift, empirical, 'Whether internet coordination scale creates qualitatively new harms that shift the constraint''s type.').

omega_variable(
    speaker_positionality_asymmetry,
    'Does the absolutist boundary have different extractiveness depending on the speaker''s power and exit options? Do powerful speakers gain more from the boundary than powerless speakers?',
    'Comparative analysis of how the boundary protects institutional speakers (universities, media, civil-liberties organizations) vs. individual speakers, and how it protects popular speakers vs. marginalized ones. Measure the asymmetry of benefit by power level: do institutional speakers and powerful individuals gain more protection and face lower reputational risk for harmful speech than powerless speakers facing targets with resources for counter-speech?',
    'If the boundary''s benefits are asymmetrically distributed by power (institutional speakers gain more), the constraint is less a pure coordination and more a power-amplifying mechanism—it amplifies the voice of those already powerful while leaving the powerless vulnerable to target retaliation. This affects per-seat directionality: a powerful speaker benefits from the boundary; a powerless speaker may be trapped by it (vulnerable both to more-powerful speakers'' harmful speech and to target retaliation if they speak back). This is the inter-seat asymmetry that refines the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_positionality_asymmetry, empirical, 'Whether the absolutist boundary''s benefits distribute asymmetrically by speaker power.').

omega_variable(
    reading_foreclosure_possibility,
    'Does the absolutist reading''s core premise (speaker autonomy is foundational) logically foreclose the dignity reading''s core premise (personhood is foundational and personhood-denying speech violates it)?',
    'Philosophical and jurisprudential analysis: can a framework hold both ''speaker autonomy is foundational'' and ''personhood is foundational'' simultaneously? Do they compete for the status of foundational, or can they coexist at different levels (autonomy is foundational for speech law; personhood is foundational for human rights law)?',
    'If foreclosure is genuine (the premises directly contradict), the reading_relations entry for the dignity_reading is ''forecloses''. If both can coexist in different institutional contexts or legal domains, the relation is ''coexists_with''. This affects whether the kernel represents a genuine binary choice or a competition among multiple holdable positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether speaker autonomy and personhood are foundationally incompatible in legal doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__absolutist_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__absolutist_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__absolutist_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__absolutist_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__absolutist_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__absolutist_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__absolutist_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__absolutist_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__absolutist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__absolutist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__absolutist_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__absolutist_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__absolutist_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__absolutist_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__absolutist_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech-harm boundary kernel, which decomposes into three structurally distinct constraints: absolutist_reading (near-absolute speaker protection, narrow unprotected categories), harm_balancing_reading (speaker protection presumptive but yields to demonstrated harm), and dignity_reading (speaker protection subordinate to human dignity, personhood-denying speech categorically unprotected). Each reading instantiates a different ε, beneficiary/victim structure, and type classification. The absolutist reading influences both siblings by setting a high bar they must overcome; harm-balancing and dignity readings coexist with the absolutist reading across different institutional and philosophical positions. Family linking: all three stories are siblings in the speech-harm-boundary kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
