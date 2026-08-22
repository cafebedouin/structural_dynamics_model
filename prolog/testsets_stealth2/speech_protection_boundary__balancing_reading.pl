% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Case-by-Case Balancing Regime for Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   Within the speech_protection_boundary kernel, this story instantiates the
 *   balancing_reading: speech protection is determined case-by-case as courts
 *   weigh First Amendment interests against competing constitutional values
 *   and demonstrated harms. The protected/unprotected boundary therefore
 *   shifts with context, coded speech and systemic harm receive intermediate
 *   scrutiny, and gatekeeping authority is distributed across the judiciary
 *   rather than fixed in categorical rules. Per the epsilon-invariance
 *   principle, the absolutist_reading and harm_limited_reading are separate
 *   constraints (separate files, linked via network.affects_constraints) with
 *   their own epsilon values, victim sets, and classifications; nothing about
 *   them is averaged into this story. The epsilon referent here is the
 *   standing arrangement under contest — the operating case-by-case weighing
 *   regime as it actually functions — assessed by this reading's own lights,
 *   never the categorical regime this reading's critics prefer. Structurally
 *   the arrangement shows both a genuine coordination function (a repeatable
 *   procedure for hard cases where categorical rules misfire in both
 *   directions) and asymmetric extraction (unpredictability costs borne by
 *   speakers, discretion and caseload centrality collected by the judiciary),
 *   which is why the claimed type is tangled_rope.
 *
 * KEY AGENTS:
 *   - - appellate_judiciary: Agenda setter (institutional/identity_locked) — administers the weighing regime and collects its discretion
 *   - - government_regulators: Primary beneficiary (institutional/constrained) — gains a case-specific litigation path to restriction
 *   - - dignity_equality_litigants: Secondary beneficiary (organized/constrained) — wins relief case-by-case, loses often, access resource-contingent
 *   - - ordinary_speakers: Primary target (moderate/trapped) — bears unpredictability and diffuse chill
 *   - - dissident_minority_speakers: Concentrated target (powerless/trapped) — most likely to lose balances, least able to litigate
 *   - - absolutist_jurists_scholars: Excluded counter-coalition (organized/mobile) — argues the categorical alternative, holds no vote in determinations
 *   - - constitutional_theorists: Analytical observer (analytical/analytical) — sees the full structure, collects nothing, bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Case-by-Case Balancing Regime for Speech Protection").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '0c5d99f4-c3b6-4023-bb20-f233782ba7ff').
narrative_ontology:cs_kernel_codification('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', fixed_text).
narrative_ontology:cs_authority_grounding('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', lineage).
narrative_ontology:cs_interpretation_layer_present('0c5d99f4-c3b6-4023-bb20-f233782ba7ff').
narrative_ontology:cs_reading_relation('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', speech_protection_boundary__harm_limited_reading, influences).
narrative_ontology:cs_axiom('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', foundational, protection_emerges_from_contextual_weighing).
narrative_ontology:cs_axiom_status(protection_emerges_from_contextual_weighing, holdable).
narrative_ontology:cs_axiom_grounding('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', protection_emerges_from_contextual_weighing, instrumental).
narrative_ontology:cs_axiom('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', secondary, systemic_harms_receive_intermediate_scrutiny).
narrative_ontology:cs_axiom_status(systemic_harms_receive_intermediate_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', systemic_harms_receive_intermediate_scrutiny, empirically_contingent).
narrative_ontology:cs_reference_frame('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', contextual_weighing_framework).
narrative_ontology:cs_drift_state('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', contemporary_tiered_scrutiny_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c5d99f4-c3b6-4023-bb20-f233782ba7ff', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, appellate_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, government_regulators).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, dignity_equality_litigants).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, ordinary_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, dissident_minority_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges on intermediate appellate courts and the supreme court decide each speech dispute by weighing the expressive interest against competing constitutional values and demonstrated harms. Their written opinions move the line between protected and regulable expression case by case. The court's institutional role, its prestige, and the significance of its docket all rest on retaining this adjudicative function; stepping away from it would mean repudiating the method their own precedents are written in.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Attorneys general, administrative agencies, and legislative counsel pursue restrictions on specific expression — election communications, threats, harassment, security-sensitive material — knowing each proposal survives or falls according to how a court weighs it. The procedure gives them a litigation route to outcomes a fixed rule would never grant; their success rate depends on selecting cases with sympathetic facts.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, government_regulators, beneficiary,
    institutional, biographical, constrained, national).

% Plaintiffs and advocacy organizations seeking relief from harassing, discriminatory, or systematically damaging expression bring claims that receive full hearing under intermediate scrutiny. Wins arrive case by case and depend on litigation resources; losses are common, and each loss hands the next defendant a favorable citation. Their access to the procedure is real but contingent.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, dignity_equality_litigants, beneficiary,
    organized, biographical, constrained, national).

% People commenting on public affairs cannot know in advance whether what they say will be protected, because protection attaches only after a court weighs the case. Many respond by narrowing their statements, avoiding contested topics, or deleting posts preemptively. There is no way to speak on public questions outside the reach of the adjudicative order.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, ordinary_speakers, payer,
    moderate, immediate, trapped, national).

% Speakers whose views are unpopular, or whose expression is coded or indirect, face the highest likelihood that a court will find the countervailing values outweigh their expressive interest. They rarely have resources to defend themselves in litigation and absorb the largest share of anticipatory silence. Alignment with absolutist jurists is possible in principle but rare in practice, since the two groups want opposite things from speech law in other respects.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, dissident_minority_speakers, payer,
    powerless, biographical, trapped, national).

% Judges writing separately and academics committed to near-absolute protection publish dissents, treatises, and model statutes arguing that fixed categorical rules outperform open-ended weighing. They argue forcefully in public and professional forums but cast no vote in the majority determinations that actually move the line; their influence runs through appointment politics and long-run doctrinal argument.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_jurists_scholars, excluded,
    organized, generational, mobile, national).

% Scholars across the interpretive spectrum code outcomes, measure directional patterns in who wins speech cases, and model how the line moves over time. They collect nothing from the procedure and bear none of its uncertainties; their seat is purely analytical.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, appellate_judiciary).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves conflicts between expressive liberty and competing constitutional values (public safety, dignity, equality, electoral integrity, national security) in cases where categorical rules would misfire in both directions — providing a shared adjudicative procedure that lets courts issue decisions in hard cases instead of defaulting to all-or-nothing answers.
% TRANSFER_FUNCTION: Moves outcome-determinative discretion from pre-committed rules to sitting judges on a case-by-case basis; moves legal protection away from speakers whose expression loses a particular weighing and toward the governmental or community interests that prevailed; moves predictability away from all speakers as the standing price of adjudicative flexibility.
% ABSENT_VOICES: Absolutist jurists and scholars object from dissents and scholarship but hold no seat in the majority's weighing; ordinary speakers appear only as litigants with resources, so the unrepresented mass of chilled speakers is absent from the room where the line is drawn; communities lacking literacy, language access, or legal standing never enter the adjudicative conversation at all.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished overnight, several thousand precedents resting on weighing tests would lose their method; courts would need to adopt either categorical protection or harm-conditioned protection, re-adjudicating harassment, campaign-finance, national-security, and campus-speech lines; regulator litigation strategies and advocacy playbooks built around winning individual balances would be discarded wholesale.
% FOUNDING_PROBLEM: Early twentieth-century prosecutions of wartime dissent and radical literature exposed a double failure: absolute protection would shield genuinely dangerous incitement, while deferential suppression criminalized peaceful criticism. Courts needed a repeatable method for distinguishing regulable from protected expression without committing in advance to a fixed line.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars across all three readings attest that hard cases recur (harassment, misinformation, coded intimidation); even absolutist critics who deny that weighing is the right solution concede the underlying problem exists. Corroboration comes from outside the benefiting parties — academic literature, comparative-law analyses of proportionality systems, bar-association reviews — not from the judiciary whose discretion the arrangement sustains.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58: substantial but short of predatory — the primary extracted good is predictability, taken from every speaker and converted into adjudicative flexibility for the bench and litigation opportunity for regulators and litigants. Suppression is 0.48: structural compulsion (no one may opt out of the adjudicative order) combined with internalized anticipatory self-censorship; it is authored as a raw structural property and is not scaled by scope or directionality — the engine owns that arithmetic. Theater is 0.32: most balancing analysis is genuine craft, but a growing share of weighing language decorates outcomes reached on other grounds, and the rhetoric of open-minded weighing performs a neutrality the outcome distribution does not always show. Accessibility_collapse is 0.38: the sibling readings remain live, voiced, and institutionally represented — understanding this arrangement does not close off the categorical alternative, which is precisely why resistance stays high (0.60: dissents, confirmation fights, codification proposals, a durable absolutist academy). The three measurement series share one time grid (points 0-100, spanning roughly the interwar emergence of weighing through the present digital-speech era) so every metric is authored at every examined point. Suppression_requirement is authored because the story specifically tracks enforcement-capacity change: the adjudicative machinery's active force grew as weighing review expanded from political-dissent cases into harassment, campaign finance, national security, and platform-adjacent disputes. Seat divergence: the judiciary seat computes a coordination-preserving arrangement it staffs and benefits from; speaker seats compute an unpredictable gauntlet whose costs concentrate on the least resourced; the engine derives these per-seat types from the structural data, and the divergence between them is the finding, not noise.
 *
 * PERSPECTIVAL GAP:
 *   From the bench, case-by-case weighing is the maturation of constitutional craft: each hard case gets the attention its facts deserve, and rigid categories are avoided. From the regulator's office, the same procedure is a usable door — propose a restriction, litigate the balance, win the cases the facts favor. From the ordinary speaker's position, the identical structure is an ex-post lottery: protection exists only after a court weighs it, so the rational strategy is to say less. Same legal order, same cases, three different lived arrangements — the gap is structural (position in the transfer flow and exit options), not informational.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary sits nearest the beneficiary pole: it administers the procedure, collects discretion, prestige, and caseload centrality, and its professional identity is fused with the method. Government regulators also sit low: they receive a case-specific path to restriction unavailable under categorical rules. Dignity-equality litigants derive as beneficiaries but sit nearer the middle than role alone suggests: they win some balances and lose others, access is resource-contingent, and each loss narrows the next claimant's prospects — their net position is contingent, not subsidized. Ordinary speakers sit high: they fund the arrangement with predictability and receive protection only contingently. Dissident and minority speakers sit nearest the full-target pole: trapped in compulsory jurisdiction, least resourced, and statistically most likely to have their expression weighed against strong countervailing values. No directionality overrides are authored: the derivation chain produces accurate values from the declared beneficiary/victim structure and exit options, and the available override granularity (keyed by power atom) is too coarse to differentiate the same-power seats that matter here (litigants versus absolutist scholars, both organized) — the engine's per-seat computation from roles handles that differentiation. National spatial scope makes verification of neutrality harder and scales effective extraction modestly upward for target seats through the engine's own modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem (a repeatable method for hard speech cases) remains live, corroborated from outside the benefiting parties. The classification work here is preventive in both directions. Reading the arrangement as pure rope would erase the asymmetric transfer — predictability flows out of every speaker while discretion concentrates in the bench — and would license indefinite expansion of weighing review. Reading it as pure snare would erase the real coordination function: categorical rules demonstrably misfire at both extremes (shielding incitement, criminalizing dissent), and the weighing procedure solves a problem each sibling solves only by accepting different failure modes. Tangled rope holds both truths: genuine coordination, real extraction, active enforcement required to sustain the asymmetry. Identity-lock dynamics bind the agenda-setter seat: the judiciary's exit is identity_locked because the method is constitutive of judicial craft identity — abandoning weighing would repudiate the precedent corpus judges authored and the self-conception of case-tailored judgment; if that identity frame broke (a court movement adopting categorical rules wholesale), the arrangement's persistence would collapse quickly despite its prohibitive formal fixing cost. If intermediate scrutiny crystallizes into a fixed tier (omega intermediate_scrutiny_crystallization), the extraction component decays toward rope; if directional skew proves systematic (omega balancing_directional_skew), the coordination story thins toward cover and the residual trends toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the speech_protection_boundary kernel (reading: balancing_reading). Which reading — balancing, absolutist, or harm_limited — will govern the standing arrangement?',
    'Doctrinal evolution tracked through landmark rulings, judicial appointments, and the ratio of categorical-rule adoptions to weighing-based determinations in speech cases.',
    'If the absolutist reading prevails, this constraint''s epsilon collapses toward negligible (few regulable cases, fixed boundary); if the harm_limited reading prevails, epsilon rises substantially (protection conditioned on harm-absence, larger victim set). The balancing reading''s intermediate position is stable only while neither sibling captures the court.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the speech-protection kernel governs; siblings instantiate different constraints with different victim sets and extraction profiles.').

omega_variable(
    disagreement_location_boundary_fixity,
    'Where exactly do the readings disagree structurally: is the protected/unprotected boundary fixed ex ante by categorical rules, or determined ex post by case-specific weighing, and which institution holds gatekeeping authority over the line?',
    'Comparative doctrinal analysis: catalog whether each reading assigns the boundary to pre-committed rules (absolutist), harm thresholds (harm_limited), or per-case judicial weighing (this reading), and trace which institution each empowers.',
    'The disagreement is located in boundary-fixity and gatekeeping distribution, not in the value of speech itself; resolving it determines whether the judiciary retains concentrated discretion (this reading) or cedes it to fixed rules (absolutist) or harm criteria (harm_limited).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_boundary_fixity, conceptual, 'Structural locus of the kernel contest: boundary fixity and gatekeeping distribution.').

omega_variable(
    chilling_effect_magnitude,
    'How large is the anticipatory self-censorship produced by protection that cannot be known in advance?',
    'Survey and experimental data correlating speech avoidance with perceived doctrinal unpredictability; natural experiments from periods or jurisdictions operating under more categorical standards.',
    'Higher measured chill raises effective extraction on speaker seats and pushes the computed classification toward the snare side; negligible chill supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Magnitude of the chilling effect arising from ex-post determination of protection.').

omega_variable(
    balancing_directional_skew,
    'Does the weighing procedure operate neutrally across cases, or does it systematically favor governmental and regulatory interests over expressive ones?',
    'Outcome coding of speech-balancing decisions across the interval: win rates for speakers versus regulators, controlling for case type and forum.',
    'A persistent pro-regulation skew converts the framework from neutral procedure into directional extraction machinery, supporting reclassification toward snare and undermining the judiciary''s neutrality claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_directional_skew, empirical, 'Whether the balance systematically favors regulation over expression.').

omega_variable(
    intermediate_scrutiny_crystallization,
    'Will intermediate scrutiny for coded speech and systemic harm stabilize into a new fixed categorical tier — dissolving this reading into a hybrid no longer describable as case-by-case weighing?',
    'Track whether intermediate-scrutiny applications converge on stable criteria applied without fresh weighing, or continue to vary with case facts.',
    'Crystallization would end the balancing reading as a distinct constraint: the boundary would again be fixed ex ante (at a middle tier), per-case judicial discretion would contract, and extraction would drop as unpredictability costs vanish.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intermediate_scrutiny_crystallization, conceptual, 'Whether intermediate scrutiny hardens into a categorical tier, ending the reading.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (compulsory jurisdiction — no speaker can opt out of the adjudicative order) or internalized (anticipatory self-censorship that persists regardless of enforcement intensity)?',
    'Compare speech behavior under equivalent doctrinal uncertainty across settings differing in enforcement salience; if chill persists where enforcement recedes, the internalized share dominates.',
    'If largely internalized, reducing enforcement would not reduce experienced suppression — the cost survives the arrangement''s own weakening, raising persistence estimates and complicating reform projections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the constraint''s suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spb_balancing_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spb_balancing_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(spb_balancing_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(spb_balancing_tr_t60, speech_protection_boundary__balancing_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(spb_balancing_tr_t80, speech_protection_boundary__balancing_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(spb_balancing_tr_t100, speech_protection_boundary__balancing_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(spb_balancing_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spb_balancing_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(spb_balancing_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(spb_balancing_be_t60, speech_protection_boundary__balancing_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(spb_balancing_be_t80, speech_protection_boundary__balancing_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(spb_balancing_be_t100, speech_protection_boundary__balancing_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spb_balancing_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(spb_balancing_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(spb_balancing_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(spb_balancing_su_t60, speech_protection_boundary__balancing_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement(spb_balancing_su_t80, speech_protection_boundary__balancing_reading, suppression_requirement, 80, 0.46).
narrative_ontology:measurement(spb_balancing_su_t100, speech_protection_boundary__balancing_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question 'how protected is speech?' covers three structurally distinct arrangements. The absolutist reading fixes the boundary ex ante near total protection (negligible extraction from its own lights); the balancing reading (this file) determines the boundary ex post through weighing (moderate-substantial extraction: unpredictability taken from speakers, discretion accrued by the bench); the harm_limited reading conditions protection on harm-absence (largest victim set, highest extraction from a speech-liberty seat). Upstream/downstream: the absolutist reading is historically prior and supplies the baseline the other two define themselves against; the balancing reading's demonstrated-harms factor feeds the harm-limited reading's premises. Each member links the others via network.affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
