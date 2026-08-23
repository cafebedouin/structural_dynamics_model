% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech-Harm Boundary, Dignity Reading: Personhood-Denying Speech Categorically Unprotected
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This story instantiates the dignity reading of the speech-harm boundary
 *   kernel: the constitutional arrangement — pioneered by the post-war German
 *   Basic Law and echoed in French memory legislation, EU framework
 *   decisions, and several other constitutions — that subordinates free
 *   expression to human dignity and places personhood-denying speech
 *   (atrocity denial, group incitement, collective defamation) wholly outside
 *   legal protection. The arrangement is presented by its maintainers as the
 *   precondition of equal citizenship; it is contested by free-expression
 *   advocates as a categorical abridgment. This file generates ONLY the
 *   dignity reading as a clean, epsilon-invariant constraint: the sibling
 *   readings (absolutist_reading, harm_balancing_reading) are separate
 *   stories with their own epsilon values and are not averaged into this one.
 *   The claim and the metrics are independent authored facts: claimed_type is
 *   tangled_rope because the arrangement pairs a genuine
 *   protective-coordination function with heavy asymmetric imposition on the
 *   speaker class under active enforcement; the metrics are authored from the
 *   arrangement's observable operation, not tuned to the claim.
 *
 * KEY AGENTS:
 *   - constitutional_dignity_state: Agenda-setter (institutional/constrained) — legislates, prosecutes, and adjudicates the dignity boundary; bound by entrenched, sometimes unamendable constitutional commitment
 *   - members_of_targeted_groups: Primary beneficiary (moderate/constrained) — receive statutory personhood protection and civic standing
 *   - general_citizenry: Beneficiary (organized/constrained) — inhabits the stabilized public sphere; bears indirect costs of enforcement
 *   - adjacent_public_speakers: Dual-positioned beneficiary/payer (moderate/constrained) — historians, journalists, satirists near the boundary; protected space plus self-censorship pressure
 *   - identity_harm_speakers: Primary payer (powerless/identity_locked) — producers of denial, incitement, and defamation; face criminal exposure
 *   - extremist_political_parties: Payer (organized/constrained) — parties trafficking in group defamation under ban proceedings and funding threats
 *   - platform_content_operators: Payer with administrative role (powerful/arbitrage) — mandated removal infrastructure; compliance cost offset by regulatory moat
 *   - free_expression_advocates: Excluded voice (organized/mobile) — hold a premise the domestic framework cannot admit; operate through international and foreign venues
 *   - international_rights_bodies: Analytical observer (institutional/analytical) — regional courts certifying the arrangement's compatibility with protected expression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.76).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech-Harm Boundary, Dignity Reading: Personhood-Denying Speech Categorically Unprotected").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4').
narrative_ontology:cs_kernel_codification('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', fixed_text).
narrative_ontology:cs_authority_grounding('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', lineage).
narrative_ontology:cs_interpretation_layer_present('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4').
narrative_ontology:cs_reading_relation('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', speech_harm_boundary__harm_balancing_reading, forecloses).
narrative_ontology:cs_axiom('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', foundational, human_dignity_trumps_expression).
narrative_ontology:cs_axiom_status(human_dignity_trumps_expression, holdable).
narrative_ontology:cs_axiom_grounding('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', human_dignity_trumps_expression, deontological).
narrative_ontology:cs_axiom('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', foundational, personhood_denial_categorically_unprotected).
narrative_ontology:cs_axiom_status(personhood_denial_categorically_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', personhood_denial_categorically_unprotected, deontological).
narrative_ontology:cs_reference_frame('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', dignity_supreme_bounded_expression).
narrative_ontology:cs_drift_state('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', contemporary_platform_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d5fc6ef-cfc9-4cce-9df1-ce31d80054e4', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, members_of_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, general_citizenry).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, adjacent_public_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, identity_harm_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, extremist_political_parties).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, platform_content_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, adjacent_public_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the prohibitions: legislatures define the punishable categories, prosecutors bring cases, courts adjudicate where expression ends and dignity violation begins. The state treats the dignity guarantee as a permanent commitment — in the paradigm jurisdiction the dignity clause is shielded from amendment altogether. Its room to relax the arrangement is bounded by that entrenchment and by coalition politics; proposing relaxation would carry severe legitimacy costs with the governing public.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, constitutional_dignity_state, agenda_setter,
    institutional, civilizational, constrained, national).

% Live under a legal guarantee that speech denying their personhood — denial of atrocities committed against them, advocacy of their exclusion, collective defamation — is criminally punishable rather than merely endured. They gain standing: participation in public life without having to litigate their own humanity in the letters page. Their protection depends on state enforcement capacity and on prosecutors prioritizing their cases; exit would mean emigration, which most cannot contemplate.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, members_of_targeted_groups, beneficiary,
    moderate, generational, constrained, national).

% Inhabits a public sphere in which the most corrosive identity-targeting speech is off the table by law, and supports the arrangement in sustained majorities. Bears the indirect costs: taxation funds enforcement, and the boundary lines occasionally catch speech citizens would defend, producing periodic controversy when artists, comedians, or politicians brush against the limits.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, general_citizenry, beneficiary,
    organized, biographical, constrained, national).

% Historians, journalists, satirists, and political critics working near the boundary. They benefit from a civic space stabilized against dehumanizing speech, and they carry self-censorship pressure: a misjudged formulation — an edgy joke, a provocative historical argument, a blunt political characterization — can trigger investigation. Most adjust phrasing rather than fight; a few litigate and win, which keeps the boundary negotiable at the margins.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, adjacent_public_speakers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, adjacent_public_speakers, payer).

% Produce or would produce the prohibited speech: atrocity denial, racial or religious incitement, group defamation. For committed movement adherents the speech is fused with worldview and self-concept, so abandoning it costs identity, not just expression. They face criminal complaint, prosecution, fines or imprisonment, and platform removal. Realistic options are silence, coded evasion, or accepting prosecution as martyrdom; a handful emigrate to jurisdictions that would host them.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, identity_harm_speakers, payer,
    powerless, biographical, identity_locked, national).

% Parties whose messaging traffics in group defamation and historical revisionism. They operate under threat of ban proceedings, candidate exclusion, and loss of public funding, and their campaign materials are treated as prosecutable speech. They can moderate rhetoric to stay inside the law — several have — but moderation dissolves the identity that binds their base, so they oscillate between provocation and retreat.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, extremist_political_parties, payer,
    organized, generational, constrained, national).

% Operate the channels where the prohibited speech now circulates. Under network-enforcement statutes they must review flagged content and remove dignity-violating material on short deadlines or face fines scaled to global revenue. They build the moderation infrastructure, absorb the compliance cost, and gain a regulatory moat — smaller rivals struggle to fund comparable review capacity. They can geofence rules by jurisdiction and tune enforcement aggressiveness within statutory bounds.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, platform_content_operators, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, platform_content_operators, agenda_setter).

% Civil-liberties organizations, constitutional scholars, and writers who hold that no category of speech should be beyond protection in principle. They litigate edge cases, publish critiques, and campaign internationally. Inside dignity-reading jurisdictions their foundational premise has no constitutional purchase — the dignity clause outranks it — so their influence runs through international bodies and foreign jurisdictions that share their premise rather than through the domestic settlement.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, free_expression_advocates, excluded,
    organized, generational, mobile, continental).

% Regional human-rights courts and treaty bodies adjudicate complaints against dignity-based speech restrictions. They have repeatedly upheld categorical dignity exclusions as compatible with protected expression, effectively certifying the arrangement's legitimacy, while insisting on procedural safeguards — precision of statute, last-resort application. They collect no rents from the arrangement; their seat is adjudicative.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared civic space in which members of vulnerable groups participate as full persons: by removing personhood-denying speech from the protected domain, the arrangement secures equal standing and forecloses the normalization dynamics that historically preceded persecution. Stated without evaluation.
% TRANSFER_FUNCTION: Moves expressive liberty — and the legal risk that accompanies exercising it — from speakers of identity-harm (deniers, inciters, group defamers) to members of targeted groups as personal security and civic standing; moves enforcement costs to the state budget and compliance costs to platform operators.
% ABSENT_VOICES: Absolutist free-expression advocates hold a premise the framework cannot admit — that no speech category is unprotected in principle — and therefore have no seat in the constitutional settlement; they operate through international and foreign venues instead. The prosecuted speakers themselves rarely shape the rule: the categories were drawn by majorities and courts, not by those the categories name.
% DISAPPEARANCE_RATIONALE: If the categorical exclusions vanished overnight, dignity-violating speech would re-enter mainstream circulation, members of targeted groups would bear the defensive burden individually rather than through the state, platform moderation rules would lose their statutory anchor and fragment by corporate policy, and the militant-democracy settlement would have to be rebuilt from contested first principles rather than administered.
% FOUNDING_PROBLEM: After the Shoah, European constitutionalism confronted the fact that unrestrained expression had been instrumental in dehumanization campaigns that made persecution administratively possible; the dignity reading was built to ensure that speech could never again serve as the preparatory machinery of personhood-destruction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: regional human-rights jurisprudence upholds dignity-based restrictions while expressly citing the historical record; historiography of wartime propaganda — produced by scholars with no stake in the arrangement — documents the mechanism the arrangement targets; and the continued activity of denial and incitement movements attests that the founding problem persists. Free-expression advocates corroborate the problem's reality while disputing the remedy, which is corroboration of the problem independent of the arrangement's self-account.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the categorical exclusion removes an entire expressive domain from the speaker class — their liberty in that domain goes to zero, not merely down — and because enforcement expansion (statutory platform mandates, EU harmonization) has extended the burden toward adjacent speakers through chilling effects; it stops short of the ceiling because the broad public retains robustly protected speech and the prohibited classes are narrowly drawn. Suppression (0.78) is predominantly structural — criminal enforcement, mandatory takedown regimes, and constitutional entrenchment that forecloses the absolutist alternative inside the jurisdiction — with a minority internalized component: adjacent speakers carry self-censorship habits that persist independent of any given prosecution; roughly 70 percent structural, 30 percent internalized. Theater is low (0.22): prosecutions are real and the protective function is exercised, though a visible minority of cases functions as remembrance ritual — fringe-figure prosecutions that signal commitment more than they reduce harm. Accessibility collapse is moderate (0.60): within a dignity jurisdiction the absolutist alternative is constitutionally foreclosed (in the paradigm case by an eternity clause), yet comparative alternatives demonstrably exist abroad and gray-zone expression persists domestically. Resistance (0.45) is real but contained: persistent boundary-testing by extremist movements, free-expression litigation, and platform lobbying, against sustained majority support. The temporal series run on one shared grid and show an enforcement ratchet — suppression_requirement climbing from episodic prosecution to systematic platform mediation — without functional decay, so no inertial-drift profile is asserted. Coalition note: the speaker class is numerous but fragmented and stigmatized; its coalition potential is low, which is why a powerless-atom seat sustains the arrangement's heaviest per-seat burden without mounting effective resistance.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply differently. From the identity_harm_speakers seat the arrangement is experienced as total silencing — a categorical foreclosure with no in-framework remedy — and that seat computes as pure extraction. From the members_of_targeted_groups seat the identical structure is experienced as security and standing. From the constitutional_dignity_state seat it is experienced as legitimate self-defense of the constitutional order itself. The platform seat is ambivalent: mandated cost plus competitive moat. Same-level dynamics: members_of_targeted_groups and adjacent_public_speakers share the moderate power atom but diverge by proximity to the boundary — the former sits deep inside the protected zone, the latter straddles it, which differentiates their exits despite equal nominal standing. Inter-institutional dynamics: the state (civilizational horizon, constitutionally constrained exit) tolerates long-run entrenchment that platforms (biographical horizon, arbitrage exit) would never accept, and international bodies (analytical exit) certify legitimacy without bearing enforcement costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the protected seats toward the beneficiary end: members_of_targeted_groups (deep beneficiary, constrained exit), general_citizenry (diffuse beneficiary, mild indirect cost), adjacent_public_speakers (beneficiary with a genuine secondary payer position — the derivation reads the secondary role and lands them intermediate). Victim declarations drive the speaker-class seats toward the target end: identity_harm_speakers (full target; identity_locked exit places them at the trapped end of the target range), extremist_political_parties (strong target, constrained exit via the option to moderate rhetoric). One directionality override is authored: power_atom powerful at d=0.45. The derivation would read platform_content_operators as a strong target because they appear in the victims array for compliance costs; that derivation is wrong for this agent because the platforms recapture much of the cost as a regulatory moat that disadvantages smaller rivals, and the enforcement labor is delegated to them under statutory mandate they helped design. Their true structural relationship is near-symmetric, slightly target-side. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that unrestrained expression served as preparatory machinery for dehumanization and persecution — remains live: denial movements persist, incitement persists, and the enforcement apparatus is expanding rather than atrophying. Mandatrophy is therefore not resolved, and no sunset is authored. The classification discipline guards both error directions: reading the arrangement as pure coordination would erase the categorical sacrifice imposed on the speaker class; reading it as pure extraction would erase the genuine protective function that sustained majorities voluntarily maintain it for. The tangled_rope claim holds both halves together. The temporal series reinforce this: theater stays low while suppression rises, indicating a hardening enforcement architecture around a functioning core, not a hollowed shell maintained by performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the speech_harm_boundary kernel; which reading a polity adopts determines the entire victim/beneficiary structure — what would change structurally under the sibling readings?',
    'Constitutional choice: adoption of a dignity-supreme clause with categorical exclusions (this reading), a near-absolute protection clause with an extreme override threshold (absolutist_reading), or a proportionality regime with no categorical exclusions (harm_balancing_reading). Comparative constitutional analysis tracks which reading each jurisdiction instantiates.',
    'Under the absolutist reading the speaker class regains presumptive protection and the protected groups lose statutory standing, redistributing extraction sharply; under the harm-balancing reading categorical exclusion dissolves into case-by-case demonstration, making the burden on any speaker context-dependent rather than fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-membership omega: this story instantiates the dignity reading; siblings instantiate incompatible boundary rules over the same kernel.').

omega_variable(
    chilling_effect_extent,
    'How far beyond the prohibited classes does the categorical exclusion reach — do historians, satirists, and political critics measurably self-censor, so that the arrangement draws cost from a far larger class than the speaker class?',
    'Prosecution-outcome studies separating bona fide prohibited speech from adjacent speech swept in, survey-based self-censorship measurement among adjacent speakers, and before/after comparison of scholarly and satirical output across enforcement-intensification episodes.',
    'Wide chilling would raise effective extraction across the moderate-power seats and broaden the asymmetry beyond the speaker class; narrow chilling confines the burden to the speaker class and stabilizes the hybrid protective/extractive profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_extent, empirical, 'Whether the categorical boundary radiates suppression into adjacent legitimate speech.').

omega_variable(
    dignity_category_expansion,
    'Does the category of personhood-denying speech expand over time — from atrocity denial toward broader offense-taking — converting a narrow categorical exclusion into a widening one?',
    'Track legislative amendments, enforcement statistics disaggregated by speech category, and appellate definitions of the prohibited classes across the interval.',
    'Category expansion compounds the burden year over year and shifts the arrangement''s center of gravity from protection toward general speech suppression; a stable narrow category keeps the protective function dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_category_expansion, empirical, 'Mission-creep risk in the definition of dignity-violating speech.').

omega_variable(
    protection_efficacy_vs_amplification,
    'Does criminalizing dignity-violating speech actually protect targeted-group members, or does prosecution amplify the prohibited messages through martyrdom and Streisand dynamics while driving circulation into harder-to-monitor channels?',
    'Longitudinal studies correlating enforcement intensity with measured harassment rates, targeted-group civic-participation indicators, and underground circulation volume.',
    'If amplification dominates, the protective coordination function is weaker than claimed and the arrangement''s burden loses its coordinating justification, pushing the computed classification toward pure extraction; if protection is real, the coordination half of the hybrid stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_efficacy_vs_amplification, empirical, 'Efficacy of the protective function versus backlash amplification.').

omega_variable(
    enforcement_selectivity,
    'Does enforcement fall evenly across the speaker class, or selectively on the powerless — fringe posters, small parties — while well-resourced or mainstream-adjacent violators escape?',
    'Compare prosecution rates and penalty severity across defendant resource levels and political positions; audit platform takedown consistency across account size and reach.',
    'Selective enforcement concentrates the effective burden on powerless seats and adds a political-filter distortion; even enforcement keeps the asymmetry defined by the rule''s text rather than by defendants'' power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether the rule''s incidence matches its text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shb_dignity_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shb_dignity_tr_t6, speech_harm_boundary__dignity_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(shb_dignity_tr_t12, speech_harm_boundary__dignity_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(shb_dignity_tr_t18, speech_harm_boundary__dignity_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(shb_dignity_tr_t24, speech_harm_boundary__dignity_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(shb_dignity_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(shb_dignity_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(shb_dignity_be_t6, speech_harm_boundary__dignity_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(shb_dignity_be_t12, speech_harm_boundary__dignity_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(shb_dignity_be_t18, speech_harm_boundary__dignity_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(shb_dignity_be_t24, speech_harm_boundary__dignity_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(shb_dignity_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(shb_dignity_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement(shb_dignity_su_t6, speech_harm_boundary__dignity_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement(shb_dignity_su_t12, speech_harm_boundary__dignity_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(shb_dignity_su_t18, speech_harm_boundary__dignity_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(shb_dignity_su_t24, speech_harm_boundary__dignity_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(shb_dignity_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the speech-harm boundary' conflates three structurally distinct claims about where expression protection ends. Each reading instantiates a different arrangement with a different epsilon, different beneficiary/victim structure, and different failure modes. This story is the dignity reading; the absolutist and harm-balancing readings are separate files linked here. The upstream/downstream structure runs through comparative constitutional citation: dignity-reading jurisprudence is cited as evidence in harm-balancing jurisdictions for the defensibility of restrictions, which is why this reading carries edges to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
