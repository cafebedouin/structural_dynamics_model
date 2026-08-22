% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection Line (Absolutist Reading — Brandenburg Standard)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The First Amendment speech clause, as interpreted since Brandenburg v.
 *   Ohio (1969), fixes the boundary of state power over speech near its
 *   maximum: advocacy may be punished only when directed to inciting imminent
 *   lawless action and likely to produce it. This file instantiates the
 *   absolutist reading of that boundary (see kernel_context); the
 *   harm-limited and balancing readings are separate constraints in separate
 *   files. The arrangement coordinates genuinely: a bright, predictable line
 *   removes case-by-case censorship discretion, shields dissent, and gives
 *   speakers reliance interests no balancing regime could match. The same
 *   arrangement externalizes: the harms of protected speech — dignitary
 *   attack, coordinated harassment, epistemic pollution — fall on minoritized
 *   communities and targeted individuals who hold no remedy and no seat in
 *   the analysis, while the protection's value concentrates in platform
 *   revenue and extremist operational space. The reading's own expected
 *   structural delta names this externality; the authored metrics describe it
 *   rather than arguing it away. Claim and metrics are independently
 *   authored: the claimed type is what the structure shows (a hybrid with a
 *   real coordination function and asymmetric accrual), and the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter (institutional/constrained) — sets and polices the line; collects no rents and bears no extraction
 *   - protected_speakers: broad beneficiary (moderate/mobile) — holds the shield
 *   - dissident_minority_speakers: intended beneficiary (powerless/mobile) — the founding justification for the line
 *   - extremist_incitement_movements: disproportionate beneficiary (organized/arbitrage) — operational space exists only inside the protection
 *   - media_platform_corporations: concentrated beneficiary (institutional/arbitrage) — monetizes the protected set and litigates to keep it
 *   - civil_liberties_organizations: beneficiary (organized/constrained) — litigates to maintain and extend the line
 *   - minoritized_target_communities: primary target (moderate/trapped) — bears the aggregate harm externality without remedy or seat
 *   - harassment_targets: target (powerless/trapped) — individuals inside coordinated protected harassment
 *   - disinformation_harmed_publics: target (moderate/constrained) — bears epistemic pollution with counter-speech as the only nominal remedy
 *   - state_legislators_regulators: payer (institutional/constrained) — bears foreclosed regulatory capacity
 *   - general_public: dual-positioned beneficiary/payer (organized/constrained) — subsidized discourse, polluted epistemics
 *   - international_human_rights_bodies: excluded (institutional/trapped) — would require dignity-harm exceptions; no seat in the doctrine
 *   - comparative_law_scholars: analytical observer (analytical/analytical) — maps the US/peer-democracy divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.55).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.6).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Near-Absolute Speech Protection Line (Absolutist Reading — Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '6cb79aa6-61ed-4466-af16-092599d3f27f').
narrative_ontology:cs_kernel_codification('6cb79aa6-61ed-4466-af16-092599d3f27f', fixed_text).
narrative_ontology:cs_authority_grounding('6cb79aa6-61ed-4466-af16-092599d3f27f', lineage).
narrative_ontology:cs_interpretation_layer_present('6cb79aa6-61ed-4466-af16-092599d3f27f').
narrative_ontology:cs_reading_relation('6cb79aa6-61ed-4466-af16-092599d3f27f', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('6cb79aa6-61ed-4466-af16-092599d3f27f', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('6cb79aa6-61ed-4466-af16-092599d3f27f', foundational, imminence_sole_unprotected_category).
narrative_ontology:cs_axiom_status(imminence_sole_unprotected_category, holdable).
narrative_ontology:cs_axiom_grounding('6cb79aa6-61ed-4466-af16-092599d3f27f', imminence_sole_unprotected_category, deontological).
narrative_ontology:cs_axiom('6cb79aa6-61ed-4466-af16-092599d3f27f', foundational, no_harm_weighting_against_speech).
narrative_ontology:cs_axiom_status(no_harm_weighting_against_speech, holdable).
narrative_ontology:cs_axiom_grounding('6cb79aa6-61ed-4466-af16-092599d3f27f', no_harm_weighting_against_speech, deontological).
narrative_ontology:cs_axiom('6cb79aa6-61ed-4466-af16-092599d3f27f', secondary, aggregate_harm_is_liberty_price).
narrative_ontology:cs_axiom_status(aggregate_harm_is_liberty_price, holdable).
narrative_ontology:cs_axiom_grounding('6cb79aa6-61ed-4466-af16-092599d3f27f', aggregate_harm_is_liberty_price, empirically_contingent).
narrative_ontology:cs_reference_frame('6cb79aa6-61ed-4466-af16-092599d3f27f', brandenburg_imminence_baseline).
narrative_ontology:cs_drift_state('6cb79aa6-61ed-4466-af16-092599d3f27f', platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6cb79aa6-61ed-4466-af16-092599d3f27f', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, protected_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, dissident_minority_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, extremist_incitement_movements).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, media_platform_corporations).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_target_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, disinformation_harmed_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_organizations).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, state_legislators_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, general_public).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, viewpoint_neutrality_principle).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, content_neutrality_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which speech restrictions survive constitutional review. Since Brandenburg v. Ohio (1969) the courts have policed a line forbidding punishment of advocacy unless it is directed to inciting imminent lawless action and likely to produce it. The line persists because these courts keep applying it: content-based restrictions that reach them are measured against it and most fail. Changing the line would require the Supreme Court to overrule its own precedents case by case; there is no other way out of that position.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Anyone who speaks or publishes on public matters holds an enforceable shield: the state must clear the imminence bar before restricting them. The shield requires no showing of value, accuracy, or civic worth — it attaches to the speech itself. Speakers rely on it when deciding what to say and publish without seeking pre-approval.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, protected_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Unpopular political advocates — socialists, civil rights organizers, anti-war groups, whistleblowers — are the class the rule was built to shield. They lack majoritarian backing and could not win case-by-case permission to speak; the categorical line is what lets them operate. Their protection is the standard exhibit in the rule's defense.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, dissident_minority_speakers, beneficiary,
    powerless, biographical, mobile, national).

% Movements whose advocacy would fail every sibling test — racist agitation, calls for violence at rallies, militia organizing — operate inside the protection so long as they stop short of directing imminent acts. The line grants them operational space no other reading would allow, and their organizing depends on it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, extremist_incitement_movements, beneficiary,
    organized, generational, arbitrage, national).

% Operate businesses built on the protected set: engagement-driven feeds that amplify inflammatory content, political advertising, monetized outrage. The near-absolute line forecloses most regulatory exposure for that content, and the companies litigate to keep it that way. Their revenue concentrates a large share of the protection's value.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, media_platform_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Litigate to maintain and extend the line, including on behalf of speakers they find repugnant. The rule's persistence is their institutional purpose; several of their landmark victories extended protection to corporate political spending and to extremist speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, beneficiary,
    organized, generational, constrained, national).

% Bear the aggregate cost of the protected set: racial and religious invective, intimidation in schools and workplaces, coordinated harassment campaigns — nearly all shielded speech. They hold no legal remedy against it and no seat in the test that shields it; their options are counter-speech, avoidance, or endurance. Their coalition power has been exercised repeatedly through proposed hate-speech statutes and campus codes, and each attempt has failed against the line. The burden compounds across generations and follows them; there is no jurisdiction to exit to.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_target_communities, payer,
    moderate, generational, trapped, national).

% Individuals targeted by protected coordinated harassment — doxxing campaigns, threatening-adjacent speech that stops short of the true-threats category, pile-ons. Exit means leaving platforms, professions, or public life entirely; the law offers them no recourse in most cases.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, biographical, trapped, national).

% Electorates and publics absorbing protected falsehoods about elections, public health, and groups. Their nominal remedy is counter-speech, which scales poorly against algorithmic amplification; regulatory remedies are foreclosed by the line they live under.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, disinformation_harmed_publics, payer,
    moderate, generational, constrained, national).

% Hold regulatory power that the line forecloses: content-based speech restrictions they enact face near-certain invalidation. Their cost is the rule's operation — they cannot answer constituent demands for hate-speech laws, disinformation rules, or harassment statutes in the forms those demands usually take.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, state_legislators_regulators, payer,
    institutional, biographical, constrained, national).

% Holds both sides of the arrangement: the shield protects their own political speech and dissent, while the same shield protects the disinformation, invective, and manipulation they absorb. They benefit from open discourse and pay in epistemic pollution; their exit from either side is limited.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, general_public, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, general_public, payer).

% Treaty bodies and peer-democracy institutions that would require exceptions for racist hate speech and dignity harm (ICCPR Article 20 and the hate-speech instruments the United States has not ratified). They have no seat in US constitutional doctrine and no enforcement path into it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, international_human_rights_bodies, excluded,
    institutional, generational, trapped, global).

% Map the divergence between US doctrine and every peer democracy's harm-limited approach, and document who bears the costs of the American line. They analyze the structure but collect nothing from it and bear none of its burdens.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, media_platform_corporations).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable legal boundary that removes case-by-case state discretion over speech content, solving the censorship-ratchet collective-action problem: speakers can rely on the line without litigating each restriction, and minority viewpoints gain a shield that majorities cannot easily move.
% TRANSFER_FUNCTION: Moves legal immunity from state restriction to speakers — disproportionately to speakers of extreme, incendiary, or monetizable harmful content — while moving the costs of that speech (dignitary attack, coordinated harassment, epistemic pollution) onto its targets, uncompensated and without a legal remedy.
% ABSENT_VOICES: The targets of protected harmful speech have no seat in the doctrine's operation: the Brandenburg test adjudicates only the speaker-state relationship, so minoritized communities bearing the aggregate harm appear, if at all, as amici — never as parties whose interests the test weighs. International human rights bodies that would require dignity-harm exceptions sit entirely outside the framework. Adherents of the harm-limited and balancing readings are present in scholarship but foreclosed from the doctrine's operative logic.
% DISAPPEARANCE_RATIONALE: The entire speech economy is organized around the line: platform content practices, movements' operational space, speakers' reliance interests, and the state's regulatory habits all presuppose near-absolute protection. If it vanished overnight, content-based regulation would flood in, platform business models would restructure around regulatory risk, extremist movements would lose their legal shield, and the harm externality would partially internalize into state discretion — with its own new distribution of harms.
% FOUNDING_PROBLEM: State punishment of unpopular political advocacy: the historical record of sedition acts, wartime prosecutions, and loyalty purges that punished abstract advocacy and association rather than imminent action.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the Alien and Sedition Acts, the Espionage Act prosecutions, and the McCarthy-era loyalty program corroborates the founding problem from outside the benefiting parties, and the recurring stream of struck-down restrictions (flag-desecration statutes, campaign-finance limits, anti-boycott laws) attests its continued liveness. No source outside the doctrine's beneficiary coalition attests the further claim that the harm externality borne by minoritized communities is a necessary price rather than a distributable cost — that justification is internal to the beneficiary set.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the externality the reading's own structural delta names — aggregate harm borne by minoritized communities without remedy — is real and has grown as the protected set filled with monetizable and extremist content, but the coordination function is genuine and large, so epsilon sits in hybrid territory rather than at pure-extraction levels. Suppression is authored at 0.60 as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope; suppression stays unscaled. The line persists through active judicial enforcement against recurring legislative attempts, and the payer seats' remedy paths are foreclosed rather than merely difficult. Theater is 0.25: the doctrine decides real cases, but the imminence test is increasingly honored in rhetoric while categorical workarounds do the deciding in hard cases. Accessibility collapse is 0.60 and asymmetric across seats: at the doctrinal level the line forecloses the harm-limitation and balancing alternatives and forecloses the payer seats' remedy paths, while the speaker seats' option space is maximized. Resistance is 0.60: sustained scholarly critique, repeated legislative attempts (the payer coalition's exercised but defeated power), and international pressure meet the line and fail against it. All three tracked metrics run on one shared eight-point grid (1969–2025) so no metric borrows another's end-state; the rising base_extractiveness series models the externality's growth with the platform era and the post-2010 corporate-speech extensions.
 *
 * PERSPECTIVAL GAP:
 *   From the federal_judiciary seat the arrangement is a principled line the Court administers and occasionally extends; from the media_platform_corporations seat it is a business asset; from the minoritized_target_communities seat the same structure operates as an uncompensated harm regime in which they have no remedy and no seat in the test that binds them. The engine computes per-seat classifications from power, exit, and role; the divergence between the agenda-setter and beneficiary seats and the payer seats is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits near symmetric: it administers the line without collecting from it. Protected speakers and dissident movements sit near the beneficiary end — the constraint subsidizes their liberty. Extremist movements and platform corporations sit at the deepest beneficiary end: their operational space exists entirely inside the protection and they hold arbitrage-grade exit (jurisdictional reach, litigation capacity, the ability to restructure around any marginal regulation). The payer seats — minoritized_target_communities, harassment_targets, disinformation_harmed_publics — sit near the full-target end: they bear the uncompensated externality with trapped or constrained exit; their coalition power is real (they pass statutes) but is defeated by the line's enforcement, which is why exit stays trapped despite moderate organizational capacity. State legislators bear a genuine cost — foreclosed regulatory capacity — that the reading counts as the line working as intended. The general_public is honestly dual-positioned: subsidized as speakers, taxed as audiences.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. The absolutist self-presentation frames the line as pure coordination that protects all speakers equally; the hybrid classification keeps the genuine anti-censorship function visible while refusing to let it launder the externality — the victim declarations and the receipt surface force the asymmetric accrual into the record. The opposite mislabel — reading the externality as pure extraction and the line as its cover — would erase the real protection that dissidents and minority speakers draw from the line and would mispredict what disappears if it fell. Mandatrophy is not the live risk here: the founding problem (state suppression of dissent) remains live, so status and verdict align and no zombie flag is warranted. The live risk is compositional drift — the doctrine now shields platforms and extremist movements more than the dissenters it was built for — which the temporal series and the composition omega are authored to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates only the absolutist_reading of the speech_protection_boundary kernel; what would the sibling readings (harm_limited_reading, balancing_reading) change structurally if they governed instead?',
    'The sibling constraint stories themselves; cross-reading comparison of epsilon, victim sets, and stakeholder surfaces over the shared referent.',
    'Under harm_limited_reading the victim set expands to include all targets of significant dignity/equality harm and remedies attach to them; under balancing_reading the bright line dissolves into case-by-case weighing and every seat''s exit options reprice. The disagreement is located in the content of the unprotected set and in the admissibility of harm-weighing — not in the kernel''s existence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this constraint is one reading of the speech_protection_boundary kernel, not the topic itself.').

omega_variable(
    externality_necessity_vs_distribution,
    'Is the aggregate harm borne by minoritized communities a necessary price of categorical protection, or a distributable cost the beneficiary coalition has externalized?',
    'Comparative institutional analysis of peer democracies with harm-limited doctrines: whether censorship ratchets materialize there, at what rate, and at what cost to dissident speech.',
    'If the price is distributable, this reading''s epsilon rises and its balance shifts toward the extractive pole; if necessary, the measured extraction is largely coordination cost and the reading''s self-assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_necessity_vs_distribution, empirical, 'Whether the harm externality is a necessary cost of the bright line or an avoidable distributional choice.').

omega_variable(
    protected_set_composition_drift,
    'Has the protected set''s composition drifted from the founding justification (shielding persecuted dissenters) to concentrated protection of powerful speakers — platforms, corporate political spending, extremist movements — and does the absolutist reading survive that drift?',
    'Longitudinal coding of who invokes and who wins under the doctrine, by speaker type and resource level, across the interval.',
    'If the doctrine now primarily shields powerful speakers, the coordination claim weakens, the extraction profile rises, and the reading operates as a different arrangement than its 1969 instantiation — the same label over a shifted structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protected_set_composition_drift, empirical, 'Compositional drift of the protected set away from the founding beneficiary class.').

omega_variable(
    counter_speech_sufficiency,
    'Is counter-speech a workable remedy for the harms the line externalizes, or does the remedy assumption fail against coordinated harassment and algorithmically amplified falsehood?',
    'Empirical study of counter-speech outcomes against coordinated harassment campaigns and viral disinformation, including response latency and reach asymmetries.',
    'If counter-speech fails at scale, the payer seats'' effective situation is worse than the structural measure suggests and the externality is uncompensated in fact as well as in law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_sufficiency, empirical, 'Whether the doctrine''s implicit remedy for targets actually functions.').

omega_variable(
    imminence_application_fidelity,
    'Is the Brandenburg imminence requirement applied as written, or honored rhetorically while categorical workarounds (true threats, harassment carve-outs, material support, fraud) decide the hard cases?',
    'Case-level coding of speech-restriction outcomes: how often does the imminence test itself, rather than a neighboring category, dispose of the case?',
    'If the test is largely honorific, the theater ratio is understated and the line''s predictability — its main coordination claim — is weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_application_fidelity, empirical, 'Fidelity of application of the imminence standard versus doctrinal workarounds.').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel the fixed constitutional text (fixed_text codification, lineage authority) or the interpretive tradition of speech jurisprudence itself (a distributed, practice-grounded kernel)?',
    'Conceptual analysis of where drift migrates: if doctrinal change requires textual amendment the kernel is the text; if it proceeds entirely through case law the kernel is the tradition.',
    'Under the tradition framing, the drift_state reads as ordinary interpretation rather than practice_drift, and the absolutist claim that the line is fixed and stable weakens; the reading''s reference frame would need redeclaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Under-determination of the commitment-system framing: text-kernel versus tradition-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.12).
narrative_ontology:measurement_basis(spee_tr_t1969, observed).
narrative_ontology:measurement(spee_tr_t1977, speech_protection_boundary__absolutist_reading, theater_ratio, 1977, 0.14).
narrative_ontology:measurement_basis(spee_tr_t1977, observed).
narrative_ontology:measurement(spee_tr_t1985, speech_protection_boundary__absolutist_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement_basis(spee_tr_t1985, observed).
narrative_ontology:measurement(spee_tr_t1993, speech_protection_boundary__absolutist_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement_basis(spee_tr_t1993, observed).
narrative_ontology:measurement(spee_tr_t2001, speech_protection_boundary__absolutist_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement_basis(spee_tr_t2001, observed).
narrative_ontology:measurement(spee_tr_t2009, speech_protection_boundary__absolutist_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement_basis(spee_tr_t2009, observed).
narrative_ontology:measurement(spee_tr_t2017, speech_protection_boundary__absolutist_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement_basis(spee_tr_t2017, observed).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_boundary__absolutist_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(spee_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.28).
narrative_ontology:measurement_basis(spee_be_t1969, observed).
narrative_ontology:measurement(spee_be_t1977, speech_protection_boundary__absolutist_reading, base_extractiveness, 1977, 0.31).
narrative_ontology:measurement_basis(spee_be_t1977, observed).
narrative_ontology:measurement(spee_be_t1985, speech_protection_boundary__absolutist_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement_basis(spee_be_t1985, observed).
narrative_ontology:measurement(spee_be_t1993, speech_protection_boundary__absolutist_reading, base_extractiveness, 1993, 0.39).
narrative_ontology:measurement_basis(spee_be_t1993, observed).
narrative_ontology:measurement(spee_be_t2001, speech_protection_boundary__absolutist_reading, base_extractiveness, 2001, 0.43).
narrative_ontology:measurement_basis(spee_be_t2001, observed).
narrative_ontology:measurement(spee_be_t2009, speech_protection_boundary__absolutist_reading, base_extractiveness, 2009, 0.48).
narrative_ontology:measurement_basis(spee_be_t2009, observed).
narrative_ontology:measurement(spee_be_t2017, speech_protection_boundary__absolutist_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement_basis(spee_be_t2017, observed).
narrative_ontology:measurement(spee_be_t2025, speech_protection_boundary__absolutist_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(spee_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.35).
narrative_ontology:measurement_basis(spee_su_t1969, observed).
narrative_ontology:measurement(spee_su_t1977, speech_protection_boundary__absolutist_reading, suppression_requirement, 1977, 0.38).
narrative_ontology:measurement_basis(spee_su_t1977, observed).
narrative_ontology:measurement(spee_su_t1985, speech_protection_boundary__absolutist_reading, suppression_requirement, 1985, 0.41).
narrative_ontology:measurement_basis(spee_su_t1985, observed).
narrative_ontology:measurement(spee_su_t1993, speech_protection_boundary__absolutist_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement_basis(spee_su_t1993, observed).
narrative_ontology:measurement(spee_su_t2001, speech_protection_boundary__absolutist_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement_basis(spee_su_t2001, observed).
narrative_ontology:measurement(spee_su_t2009, speech_protection_boundary__absolutist_reading, suppression_requirement, 2009, 0.52).
narrative_ontology:measurement_basis(spee_su_t2009, observed).
narrative_ontology:measurement(spee_su_t2017, speech_protection_boundary__absolutist_reading, suppression_requirement, 2017, 0.57).
narrative_ontology:measurement_basis(spee_su_t2017, observed).
narrative_ontology:measurement(spee_su_t2025, speech_protection_boundary__absolutist_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(spee_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'speech protection' decomposes into three structurally distinct readings of the speech_protection_boundary kernel. This file is the absolutist reading (unprotected set = imminent incitement only; epsilon 0.55 by the reading's own lights). speech_protection_boundary__harm_limited_reading (protection conditional on absence of significant dignity/equality harm) authors substantially higher epsilon over the same referent, and speech_protection_boundary__balancing_reading (case-by-case weighing) authors an intermediate, variance-bearing epsilon. The epsilon values differ because epsilon is reading-indexed over a shared referent, not because the observables differ; each file is a separate constraint with its own victim set and stakeholder surface. The absolutist reading is the doctrinal baseline the sibling readings contest; edges are typed in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
