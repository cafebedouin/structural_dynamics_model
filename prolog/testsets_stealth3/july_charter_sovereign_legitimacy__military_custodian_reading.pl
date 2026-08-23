% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Charter-Ratified Permanent Military Custodianship (Guardian Reading)
 *   domain: political/constitutional law
 *
 * SUMMARY:
 *   A post-revolutionary constituent assembly ratifies a charter embedding
 *   the armed institution as permanent guardian: a defence council dominated
 *   by military command holds a standing veto over security-touching
 *   legislation and appointments, the defence budget is constitutionally
 *   shielded from parliamentary audit, and the text is amendable only through
 *   gates the veto itself protects. Elections continue and a legislature
 *   sits, but contestation operates inside boundaries the security apparatus
 *   polices. The colloquial label 'the charter's sovereignty settlement'
 *   covers multiple structurally distinct claims; per the epsilon-invariance
 *   principle this file instantiates exactly one of them — the
 *   military_custodian_reading — with its own epsilon, victim set, and
 *   classification, and links its siblings through
 *   network.affects_constraints. Claim and metrics are independent authored
 *   facts: tangled_rope is asserted from structure (a genuine coordination
 *   service delivered through the same channels that move asymmetric rents,
 *   held by active enforcement), while the metric values describe observed
 *   operation. Where the engine's per-seat computation diverges from the
 *   claim, that divergence is the datum the corpus exists to take. KEY AGENTS
 *   (by structural relationship): - military_high_command: agenda-setter and
 *   primary collector (institutional/arbitrage) — administers custodial
 *   articles; receives shielded budget, veto rents, enterprise dividends -
 *   military_owned_enterprises: secondary collector (organized/arbitrage) —
 *   captive contracts, exemptions, uninspectable welfare funds -
 *   international_security_patrons: external collector
 *   (institutional/arbitrage) — buy stability, bear no domestic cost -
 *   autonomous_political_parties: primary bearer (organized/identity_locked)
 *   — dissolution decrees, screened platforms - university_student_movement:
 *   primary bearer (powerless/trapped) — supervised campuses, arrest cycles -
 *   civilian_oversight_bodies: secondary bearer (moderate/constrained) —
 *   denied audit access, state-secrets prosecutions -
 *   exiled_opposition_figures: excluded voice (moderate/trapped) — barred
 *   from the constituent record - civil_military_relations_analysts:
 *   analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.62).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.76).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Charter-Ratified Permanent Military Custodianship (Guardian Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "political/constitutional law").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'f5a6bc9b-694c-4d06-9539-c45af1a870d1').
narrative_ontology:cs_kernel_codification('f5a6bc9b-694c-4d06-9539-c45af1a870d1', fixed_text).
narrative_ontology:cs_authority_grounding('f5a6bc9b-694c-4d06-9539-c45af1a870d1', extraction).
narrative_ontology:cs_interpretation_layer_present('f5a6bc9b-694c-4d06-9539-c45af1a870d1').
narrative_ontology:cs_reading_relation('f5a6bc9b-694c-4d06-9539-c45af1a870d1', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('f5a6bc9b-694c-4d06-9539-c45af1a870d1', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('f5a6bc9b-694c-4d06-9539-c45af1a870d1', foundational, permanent_guardianship_above_elected_institutions).
narrative_ontology:cs_axiom_status(permanent_guardianship_above_elected_institutions, holdable).
narrative_ontology:cs_axiom_grounding('f5a6bc9b-694c-4d06-9539-c45af1a870d1', permanent_guardianship_above_elected_institutions, instrumental).
narrative_ontology:cs_axiom('f5a6bc9b-694c-4d06-9539-c45af1a870d1', secondary, electoral_contestation_within_security_bounds).
narrative_ontology:cs_axiom_status(electoral_contestation_within_security_bounds, holdable).
narrative_ontology:cs_axiom_grounding('f5a6bc9b-694c-4d06-9539-c45af1a870d1', electoral_contestation_within_security_bounds, conventional).
narrative_ontology:cs_reference_frame('f5a6bc9b-694c-4d06-9539-c45af1a870d1', charter_ratified_permanent_guardianship).
narrative_ontology:cs_drift_state('f5a6bc9b-694c-4d06-9539-c45af1a870d1', decade_after_ratification, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f5a6bc9b-694c-4d06-9539-c45af1a870d1', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, international_security_patrons).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, university_student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_oversight_bodies).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, guardian_vanguard_theory).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratifies and administers the charter's security articles: chairs the national defence council holding a standing veto over legislation and appointments touching defense, sovereignty, and the security services; sets the defense budget behind a constitutional shield exempting its ledgers from parliamentary audit; appoints the defense minister and provincial governors; issues dissolution decrees against parties and closures against campuses when contestation crosses stated bounds. Receives the shielded budget share, council perquisites, and enterprise dividends routed through officer welfare funds. Personnel rotate between commands, ministries, and corporate boards, so the institution's position does not depend on any individual's tenure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, beneficiary).

% Conglomerates spanning construction, food processing, retail, and broadcast operate under officer ownership structures: they win state tenders without competitive bidding, hold tax exemptions, employ conscripted labor, and channel profits to welfare funds the civilian treasury cannot inspect. Managers sit on chambers of commerce and bankroll sympathetic media. Were the security articles opened to amendment, their contract advantages and exemptions would become ordinary legislation subject to repeal.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_owned_enterprises, beneficiary,
    organized, biographical, arbitrage, national).

% Registered parties compete in scheduled elections, but their platforms are screened: any plank touching the defense budget, the services' prerogatives, or the charter's security articles invites dissolution decree, leadership detention, or registration revocation. Several founding parties have been dissolved and re-founded under new names; their leaders cycle between parliament, house arrest, and prison. Withdrawing from politics would mean abandoning members and constituencies who organize their lives around the parties — most cadres treat exit as betrayal rather than an available option.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, identity_locked, national).

% Student unions historically anchored national protest waves; campuses now host security-liaison offices, union elections are supervised, and organizers face suspension, conscription retaliation, and arrest cycles timed to examination seasons. Participation costs transcripts and produces detention records that follow graduates into hiring. An individual can graduate and leave; the movement's constituency has nowhere to relocate.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, university_student_movement, payer,
    powerless, immediate, trapped, national).

% The parliamentary budget committee and the supreme audit agency hold formal jurisdiction over public accounts, yet defense ledger lines arrive classified or aggregated; requests for line-item review draw sovereignty exceptions, and committee chairs who persist lose assignments or face prosecution under state-secrets statutes. Individual members may resign into private life; the institutions themselves cannot compel disclosure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_oversight_bodies, payer,
    moderate, biographical, constrained, national).

% Foreign governments and blocs treat the armed institution as the region's single reliable counterterrorism and border-security partner; they extend assistance, training, and debt relief conditioned little on domestic civil-military arrangements. They gain a predictable partner and an arms market while bearing none of the domestic costs, and they can redirect assistance to neighboring partners at modest expense.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_security_patrons, beneficiary,
    institutional, generational, arbitrage, global).

% Founding-era opposition leaders were barred from the ratification process and from returning afterward; they publish critiques from abroad, testify to foreign legislatures, and sustain diaspora networks. Their objection — that a permanent custodian converts every future election into an administered plebiscite — never entered the constituent record. Return requires security clearance they cannot obtain.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, exiled_opposition_figures, excluded,
    moderate, biographical, trapped, global).

% Academic researchers and think-tank analysts map the settlement: budget shares, veto incidence, enterprise revenues, dissolution counts. They publish, advise foreign ministries, and occasionally brief domestic committees; they neither administer the arrangement nor bear its costs, though some face visa denial after critical publications.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civil_military_relations_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified coercive apparatus and a pre-committed veto point that prevented factional capture of the armed forces during the post-ratification window: rival blocs could not convert electoral victories into control of the army, and the state kept one chain of command through the transition.
% TRANSFER_FUNCTION: Moves a constitutionally shielded budget share, exclusive policy domains, and appointment prerogatives from civilian institutions to military command; moves the practical right of political contestation from parties and campus organizations into security-bounded channels; moves contract flow and conscripted labor to officer-owned enterprises.
% ABSENT_VOICES: Exiled opposition founders and the leadership of already-dissolved parties were barred from the constituent process; student representatives were excluded after the campus crackdowns that preceded ratification. Their objection — that permanence converts every future election into an administered contest — is recorded nowhere in the official proceedings.
% DISAPPEARANCE_RATIONALE: Overnight removal of the custodial articles dissolves the defence council's veto, returns the shielded ledger lines to audit, lets dissolved parties re-register without screening, and exposes enterprise privileges to ordinary repeal; the civil-military settlement, the party system, and campus politics all reorganize within months.
% FOUNDING_PROBLEM: At ratification the state faced rival militias, armed remnants of the old regime, a live fear of civil war between religious and secular blocs, and a civilian government with no loyal coercive instrument; the charter embedded the one disciplined armed institution as permanent guardian.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous diplomatic reporting and regional security assessments corroborate the acute founding problem as real at ratification. Opposition parties, student unions, and several comparative-politics scholars — all outside the benefiting set — attest the acute phase closed years ago and the arrangement now persists as institutional self-protection; no disinterested corroborator attests that the founding problem remains live today, and none certifies it dead either.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the arrangement moves a shielded budget share, exclusive policy domains, and enterprise margins from civilian reach into officer networks, while also delivering a real service — one command chain and a pre-committed veto that blocked factional seizure of the armed forces in the acute window. The net transfer is large but not the whole story, which is why the value sits short of pure-extraction territory. Suppression is 0.76 and is a RAW structural property, unscaled by power or scope: dissolution decrees, supervised campus elections, state-secrets prosecutions, and arrest cycles timed to mobilization calendars are the machinery that holds contestation inside bounds; only extractiveness is scaled downstream, by directionality and scope. Theater ratio is 0.42: elections and parliamentary sessions are real procedures with real participants, but a growing share of legislative activity arrives pre-cleared or dies at council stage, so performance increasingly substitutes for decision. Accessibility collapse is 0.58: the amendment path runs through the veto it would remove, collapsing the obvious exit, while judicial reinterpretation and patron pressure leave partial openings. Resistance is 0.63: recurring protest waves, serial re-founding of dissolved parties, and audit-committee defiance meet the arrangement continuously. The three measurement series share one grid (0, 4, 8, 12, 16, 20, 24) so no metric silently borrows another's end-state: extractiveness accumulates as rents layer onto the original settlement, theater climbs as procedural substitutes spread, and the suppression requirement hardens through the mid-interval enforcement build-out before plateauing — an enforcement-infrastructure trajectory, not mere extraction shift, which is why the suppression series is authored at all.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the agenda-setter seat the settlement reads as the coordination it built and staffs: budgets defended, borders held, factions contained. From the party seat the same structure is a bounded plebiscite machine; from the student seat, an arrest calendar; from the auditor seat, a wall of classification stamps. Same-level divergence is structural too: the parties and the student movement sit on the same paying side at different power atoms, yet their exits differ — the parties are ideologically fused to their constituencies (leaving means abandoning the members who constitute them), while students are individually replaceable but collectively stationary (the constituency cannot emigrate from the country it organizes). A parties-plus-students coalition is the arrangement's chief vulnerability, and the enforcement calendar anticipates it: arrest waves land between electoral cycles and exam seasons, decapitating joint mobilization before it forms. Patrons experience the arrangement entirely from outside its costs, which is how a beneficiary seat computes near-benign while every domestic payer seat computes extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: military_high_command (both runs and collects — d near the subsidized end), military_owned_enterprises (collects without administering), and international_security_patrons (external subsidy recipients with arbitrage-grade exit, sitting nearest the beneficiary pole). Victim declarations: autonomous_political_parties, university_student_movement, and civilian_oversight_bodies bear the transfers and the bounded-contestation costs; identity-locked and trapped exit profiles push the parties and students toward the full-target end, while the auditors' constrained-but-real resignation option moderates theirs slightly. The exiled_opposition_figures seat is authored as excluded: per the ruling on authored absences, exclusion is commentary-grade evidence about how the consensus was manufactured, and must never feed a classification override. The analyst seat observes. Domestic seats carry national scope; patrons carry global scope — larger scope makes verification of the shielded ledgers harder, and the engine reflects that in scaled extraction rather than in any authored modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rival militias, no loyal coercive instrument, live fear of sectarian civil war — was real and externally corroborated at ratification. Reading the whole arrangement as pure extraction erases the coordination service that actually held the transition together; reading it as pure coordination erases the enterprise complex, the shielded ledger, and the dissolution machinery that accumulated afterward. The tangled_rope claim keeps both halves visible. The temporal series is the mandatrophy instrument: the founding problem's liveness decays across the interval while extractiveness accumulates — the signature of a mandate converting into privilege. Founding_problem_status is authored 'contested' rather than 'dead' because no disinterested corroborator certifies either pole; mandatrophy_resolved is deliberately left undeclared. The status-by-verdict mismatch routes this story toward capture/zombie investigation rather than settling it here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the military_custodian_reading of the july_charter_sovereign_legitimacy kernel; secular_democratic_reading and guided_nationalism_reading instantiate different constraints over the same charter text. What structurally switches under each sibling?',
    'Compile the two sibling stories and compare victim sets, epsilon, and computed types across the kernel family; the disagreement is localized in the locus of ultimate command authority and the permanence of the custodial veto.',
    'Under the secular-democratic sibling the officer corps becomes the target and the civilian institutions the protected party; under the guided-nationalist sibling the victim set shifts toward heterodox religious actors. Cross-reading comparison resolves the difference — intra-story hedging must not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: this story is one reading of a contested charter kernel; siblings live in separate files.').

omega_variable(
    custodial_permanence_necessity,
    'Was embedding the armed institution as PERMANENT guardian causally necessary for the stability the charter achieved, or did a transitional necessity get converted into permanent privilege once the acute fragmentation window closed?',
    'Compare post-transition states that sunset custodial arrangements against matched cases that retained them, controlling for initial fragmentation severity; within-case test whether stability indicators deteriorated in polities that later civilianized command.',
    'If transitional necessity, the permanence clause is a converted mandate and much of the measured transfer reprices from coordination-cost into rent; if genuinely necessary, part of the transfer pays for a real ongoing service and epsilon falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_permanence_necessity, empirical, 'Whether permanent guardianship tracks a live necessity or a converted transitional mandate.').

omega_variable(
    veto_domain_opacity,
    'Which policy domains does the defence-council veto actually bind, versus which are formally subject but practically untouched?',
    'Legislative tracing: classify every bill introduced since ratification by whether it reached council stage, died there, or was informally pre-cleared before introduction.',
    'Concentrated veto incidence sharpens effective extraction on the bound domains and hardens payer-seat classifications; diffuse incidence spreads it thin and softens per-seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_domain_opacity, empirical, 'Actual incidence of the custodial veto across policy domains.').

omega_variable(
    suppression_internalization_split,
    'Is bounded political contestation maintained by structural enforcement alone, or partly internalized — do party strategists and student organizers self-censor such that relaxing enforcement would not promptly restore contestation?',
    'Post-liberalization trajectories in comparable custodial states, plus elicitation of opposition elites'' revealed caution following enforcement relaxations.',
    'If substantially internalized, measured suppression understates the arrangement''s grip and outlasts any enforcement rollback; payer-seat classifications harden and the persistence prognosis worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized component of bounded contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(custodian_reading_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(custodian_reading_tr_t4, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(custodian_reading_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(custodian_reading_tr_t12, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(custodian_reading_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(custodian_reading_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(custodian_reading_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(custodian_reading_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(custodian_reading_be_t4, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(custodian_reading_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(custodian_reading_be_t12, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(custodian_reading_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(custodian_reading_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(custodian_reading_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(custodian_reading_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(custodian_reading_su_t4, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 4, 0.69).
narrative_ontology:measurement(custodian_reading_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(custodian_reading_su_t12, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(custodian_reading_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(custodian_reading_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(custodian_reading_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'the charter's sovereignty settlement' conflates three structurally distinct claims with distinct epsilon, victim sets, and classifications. This file instantiates the custodian reading; the secular-democratic and guided-nationalist readings are separate stories linked here. Upstream/downstream structure: the custodian reading structurally pressures the nationalist sibling (the guardian vets which identity claims survive) and forecloses the civilian-supremacy sibling within any single framework; the siblings must link back to this ID in their own network blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
