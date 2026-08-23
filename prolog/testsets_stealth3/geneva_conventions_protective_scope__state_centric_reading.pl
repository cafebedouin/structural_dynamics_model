% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric Reading (Article 4 Criteria Gate)
 *   domain: international_humanitarian_law/legal_theory
 *
 * SUMMARY:
 *   The 1949 Geneva Conventions encode a protective architecture whose scope
 *   this story reads through the state-centric lens: protections attach to
 *   combatants satisfying the Article 4 criteria — responsible command, fixed
 *   distinctive sign, open carriage of arms, observance of the laws of war —
 *   and fighters who do not present those markers fall outside the treaty's
 *   protected classes, to be handled under the detaining power's own
 *   authority. This file is ONE READING of the contested kernel
 *   geneva_conventions_protective_scope, instantiated as
 *   state_centric_reading; the sibling files
 *   geneva_conventions_protective_scope__universal_rights_reading and
 *   geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   instantiate different scope predicates over the same standing arrangement
 *   and are separate constraints with their own epsilon, victim sets, and
 *   classifications. Per the family decomposition rule the referent is shared
 *   — the Article 4-gated protective scope as it stands — while epsilon is
 *   reading-indexed: this reading adjudicates excluded fighters as
 *   legitimately outside entitlement, so it authors the family's lowest
 *   epsilon (0.50) for a referent the universal reading would score far
 *   higher. The claim/metric split is deliberate and unreconciled:
 *   claimed_type is tangled_rope because the structure pairs a genuine,
 *   still-operating interstate coordination function (reciprocal POW
 *   guarantees, protected medical and religious personnel, verifiable
 *   membership criteria both belligerents can check) with an enforced
 *   asymmetric exclusion concentrating operational latitude on conventional
 *   militaries; the metrics describe the arrangement's actual operation,
 *   including an enforcement history that intensified sharply after 2001.
 *
 * KEY AGENTS:
 *   - high_contracting_states — agenda-setter (institutional / arbitrage): sets and enforces the scope boundary through service manuals, reservations, and detention policy; collects reciprocal guarantees and interpretive control.
 *   - conventional_state_militaries — primary beneficiary (institutional / constrained): collects combatant immunity, POW reciprocity, and targeting latitude against fighters the gate excludes.
 *   - unprivileged_belligerents — primary target, targeting side (organized / trapped): engageable without combatant immunity; classification decided by the adversary.
 *   - detained_nonstate_fighters — primary target, detention side (powerless / trapped): held outside the POW register under captor-controlled rules.
 *   - civilians_in_asymmetric_conflicts — incidental beneficiary with spillover costs (powerless / trapped): shielded as civilians by the same structure that narrows around them.
 *   - icrc_and_humanitarian_agencies — monitoring observer (institutional / analytical): custodial access and documentation without adjudicative power.
 *   - human_rights_advocacy_organizations — excluded voice (organized / trapped): would contest the boundary inside treaty politics; kept outside the interpretation channels.
 *   - ihl_legal_community — analytical observer (moderate / analytical): shapes long-run interpretation without enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.5).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric Reading (Article 4 Criteria Gate)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, 'ab7835ce-42ed-4d0b-a4eb-e25f5e3083af').
narrative_ontology:cs_kernel_codification('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', fixed_text).
narrative_ontology:cs_authority_grounding('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', lineage).
narrative_ontology:cs_interpretation_layer_present('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af').
narrative_ontology:cs_reading_relation('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', foundational, combatant_privilege_requires_article_four_criteria).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_article_four_criteria, holdable).
narrative_ontology:cs_axiom_grounding('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', combatant_privilege_requires_article_four_criteria, conventional).
narrative_ontology:cs_axiom('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', foundational, excluded_belligerents_handled_under_domestic_criminal_frame).
narrative_ontology:cs_axiom_status(excluded_belligerents_handled_under_domestic_criminal_frame, holdable).
narrative_ontology:cs_axiom_grounding('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', excluded_belligerents_handled_under_domestic_criminal_frame, conventional).
narrative_ontology:cs_reference_frame('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', article_four_gated_protective_scope).
narrative_ontology:cs_drift_state('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', contemporary_asymmetric_conflict_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab7835ce-42ed-4d0b-a4eb-e25f5e3083af', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, high_contracting_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, detained_nonstate_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_conflicts).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, article_4_criteria_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, combatant_privilege_distinction).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, state_monopoly_on_lawful_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and ratified the 1949 Conventions and their protocols; interpret the protective scope through national service manuals, judge-advocate doctrine, and reservation practice; enforce the boundary through detention policy and status determinations made by their armed forces. Collect reciprocal prisoner-of-war guarantees for their own service members and retain wide latitude in how they treat fighters they classify as outside the protected classes. Can re-read the texts, adhere to or repudiate additional protocols, or attach reservations — few external penalties attach to any of these moves.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, high_contracting_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, high_contracting_states, beneficiary).

% Field armies operating under the framework day to day. Their members carry combatant immunity and a credible promise of prisoner-of-war treatment if captured, backed by the criteria gate that marks who belongs to the protected class. In campaigns against irregular opponents the same gate leaves opposing fighters outside the classes their own troops occupy, freeing targeting and detention decisions from the obligations that bind treatment of qualified forces. Leaving the framework entirely — fighting outside the laws of war — would cost them allied reciprocity and domestic legitimacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, biographical, constrained, global).

% Fighters in organized armed groups who do not present the markers the criteria require — fixed distinctive signs, open carriage of arms, command structures a detaining power chooses to recognize. Under the reading they sit outside the treaty's protected classes: they can be engaged without the immunity accorded qualified combatants, and if captured they do not enter the prisoner register. Their one nominal route in — adopting the required markers — is verified at the discretion of the power holding them, not by any body they can appeal to.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    organized, biographical, trapped, regional).

% Captured fighters held after classification outside the prisoner regime. They lack the registration, notification, and repatriation rights that attach to qualified prisoners; their treatment follows the detaining power's own rules — interrogation directives, military commissions or none, release on the detaining power's schedule alone. Nothing they do changes their classification; the decision belongs entirely to the captor.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, detained_nonstate_fighters, payer,
    powerless, biographical, trapped, regional).

% Non-combatant residents of conflict zones. The reading's distinction shields them as civilians — medical services, quarter protections, and the prohibition on targeting non-participants run through the same treaty structure. When the combatant classes narrow and irregulars are pushed outside them, the line between fighter and civilian thins in practice, and they absorb the resulting exposure in raids, detention sweeps, and indiscriminate-fire environments.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_conflicts, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, civilians_in_asymmetric_conflicts, payer).

% Hold custodial-access mandates and visit detention facilities worldwide; document the treatment of both registered prisoners and the unregistered; publish commentary urging broader application of the conventions' minimum guarantees. They attend diplomatic conferences but hold no vote in how states read the scope; their leverage is persuasion and access, not adjudication.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, icrc_and_humanitarian_agencies, observer,
    institutional, generational, analytical, global).

% Campaign for extending protections to all persons in state custody during armed conflict and document cases where classification decisions looked discretionary. They stand outside the formal interpretation channels — state-party conferences, national military manuals, service tribunals — where scope readings are actually settled; their instruments are reporting, litigation in domestic courts, and pressure on legislatures.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, trapped, global).

% Academic lawyers, treaty-body members, and former service legal officers who publish interpretations, teach the next generation of military lawyers, and draft model rules. Their readings circulate widely and slowly shift doctrine, but they command no enforcement machinery and depend on state institutions to adopt anything they propose.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, ihl_legal_community, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate collective-action problem of reciprocal treatment in war: gives each state's soldiers a credible guarantee of prisoner-of-war treatment if captured, gives medical and religious personnel protected status, and provides objective criteria both belligerents can independently verify to establish who merits protection.
% TRANSFER_FUNCTION: Moves protection and legal status according to the criteria gate: compliant uniformed forces receive immunity and registered-prisoner guarantees, while fighters failing the criteria have protection moved away from them — their engagement loses immunity constraints and their detention falls under the captor's own authority — transferring operational latitude to the state forces opposing them.
% ABSENT_VOICES: The excluded fighters themselves and their political representatives have no seat anywhere in treaty interpretation; human rights organizations and affected civilian populations articulate objections from outside the state-party conferences, national manuals, and service tribunals where scope readings are actually settled.
% DISAPPEARANCE_RATIONALE: If the criteria gate vanished overnight, detention and targeting practice would rearrange around whatever successor scope predicate applied: reciprocal prisoner guarantees underpinning surrender decisions would need renegotiation, existing classification machinery would lose its legal anchor, and every state's detention doctrine would require rewrite — the arrangements of dozens of militaries depend on the gate's current shape.
% FOUNDING_PROBLEM: After the Second World War, to guarantee that captured members of armed forces receive humane treatment and that medical and religious personnel are protected, using objective criteria — responsible command, fixed distinctive sign, open carriage of arms — so that belligerents could identify who merits protection and trust the other side's reciprocity.
% FOUNDING_PROBLEM_CORROBORATION: Partial corroboration exists from outside the benefiting parties: International Committee of the Red Cross custody documentation and the observed mechanics of interstate prisoner exchanges attest that the reciprocal-protection function remains operative, and academic international-humanitarian-law scholarship corroborates the founding problem's continuing relevance. Human rights organizations and parts of the legal community attest from outside the beneficiary set that the arrangement's persistence in asymmetric practice now leans substantially on the exclusion function rather than the protection function. No corroborating voice exists from the excluded fighters themselves — they have no institutional seat in treaty interpretation, and that absence is itself signal.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.50 is authored from this reading's own lights over the standing arrangement: the reading endorses criteria-gating as principled (protections track observable compliance, which it regards as merit rather than extraction), but it concedes that the gate still transfers protection away from identifiable persons — fighters who can be engaged without immunity and detainees held outside the register — to state operational freedom, and that observed detention practice has outrun the reading's own criminal-handling frame. That yields moderate, not low, extraction: systematically enforced exclusion with real cost-bearers, discounted by the reading's entitlement adjudication. Suppression 0.62 is a raw structural property, unscaled by power or scope per the framework rule: the boundary is maintained by active machinery — status determination boards, military commissions, doctrinal manuals, captor-controlled classification — not by participant preference. Theater 0.32: the criteria-screening function is real and still sorts people, but a growing share of activity is performative (tribunals producing predetermined determinations, compliance reporting that never revisits classifications). Accessibility_collapse 0.55 blends two divergent pictures: state parties retain live alternatives (voluntary Common Article 3 application, protocol adherence, reservation withdrawal), while excluded fighters' alternatives collapse almost completely since classification sits with the captor. Resistance 0.65: sustained contest from humanitarian agencies, advocacy organizations, parts of the legal community, and some state parties championing broader readings. Temporal data run on ONE shared grid (1949, 1959, 1969, 1977, 1987, 2001, 2011, 2025) across all three tracked metrics; the trajectories trace a quasi-cyclical enforcement ratchet — suppression and extraction surge with each asymmetric campaign wave (Kenya/Algeria-era counterinsurgency, Vietnam, the post-2001 detention program) and plateau rather than fully relax between waves, so the oscillation is a ratchet, not intermittent reinforcement. End-state values match the base_properties scalars. Receipt surface: gains demonstrably accrue to military operational and detention commands (named seat conventional_state_militaries — receipt is not the same fact as beneficiary role, since high_contracting_states also benefit but the gains land operationally); fixing is prohibitive for the fixing seat (state parties) because abandonment carries concentrated operational costs in ongoing asymmetric campaigns while the benefits of fixing are diffuse. Suppression here is structural — legal and institutional barriers — not internalized cognition.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats should diverge sharply. From the agenda-setter and beneficiary seats the arrangement presents as an earned-membership order they built, defend, and profit from: criteria are objective, reciprocity is real, and exclusion is the price of violating the laws of war. From the payer seats the identical structure operates as captor discretion: nothing a fighter does changes their classification, verification belongs to the adversary, and detention conditions follow the captor's rules alone. Civilians compute near-symmetric — shielded by the same distinction that narrows around them. Same-level divergence: two states of equal sovereign standing experience opposite directionalities depending on force posture — a military facing irregular opponents collects large operational gains from the gate, while one in interstate posture collects mainly the reciprocal POW guarantee; equal global power, different exits, different experienced constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: high_contracting_states and conventional_state_militaries sit near the beneficiary end (subsidized by the gate), with the states' arbitrage-grade exit pushing them furthest toward it. Victim declarations map to high directionality: unprivileged_belligerents and detained_nonstate_fighters sit near the full-target end, with trapped exit reinforcing it — a fighter cannot opt into protections, and a detainee's classification is wholly captor-controlled. civilians_in_asymmetric_conflicts derive mid-range: declared beneficiaries of the civilian protections, carrying real spillover costs as the combatant classes narrow. No directionality_overrides are authored: the derivation chain from declared roles plus exit options already separates the seats correctly, and overrides are keyed by power atom, so a single correction would collide across seats sharing an atom (powerless covers both detained fighters, who are near-full targets, and civilians, who sit mid-scale — one override per atom would corrupt one of them). Excluded and observer seats feed commentary, not the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guaranteeing captured lawful combatants humane treatment through reciprocal, verifiable criteria — is not dead: interstate POW exchanges still run on the Third Convention's mechanics, and the reciprocal guarantee visibly shapes surrender decisions in current interstate wars. But the arrangement's persistence in asymmetric practice now leans heavily on the exclusion function, which is why founding_problem_status is authored contested rather than live. The tangled_rope claim prevents both mislabels: calling this a pure rope launders an enforced exclusion behind the genuine reciprocity function; calling it a pure snare erases a coordination function that observably still delivers protections in interstate conflict. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and correctly fires no zombie flag — the protection function is alive even as the exclusion function consolidates. The mandate has not outlived the arrangement, so no mandatrophy_resolved declaration is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the state_centric_reading of kernel geneva_conventions_protective_scope: what structurally changes if a sibling reading adjudicates the same standing arrangement instead?',
    'Comparative classification across the three reading-files sharing the referent (this file, universal_rights_reading, hybrid_proportionality_reading): differences in authored victim sets and epsilon locate the disagreement.',
    'The universal_rights_reading enlarges the protected class to all persons affected, converting excluded fighters from legitimately-outside to protected-population and raising the epsilon authored for the same referent; the hybrid_proportionality_reading indexes scope to conflict type, splitting this story''s seat structure by international vs non-international conflict. The disagreement is located in the scope predicate itself: individual status criteria vs universal personhood vs conflict-type scaling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer position of this story within the protective-scope kernel contest.').

omega_variable(
    criteria_objectivity_screen,
    'Do the Article 4 criteria operate as objective conduct screens in asymmetric conflict, or as discretionary filters whose application tracks which side the fighter fights for?',
    'Cross-conflict audit of status determinations: correlate criteria-meeting (uniform, open arms carriage, recognizable command) with protection actually granted, controlling for adversary identity and detaining power.',
    'If determinations filter on affiliation rather than conduct, the criteria gate functions as discretionary exclusion dressed as principle and the arrangement drifts toward pure extraction; if determinations track conduct, the gate retains earned-membership character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_objectivity_screen, empirical, 'Objectivity of the criteria screen under asymmetric-conflict conditions.').

omega_variable(
    criteria_adoption_escape_route,
    'Can excluded armed groups escape their position by meeting the criteria (adopting fixed signs, open carriage, responsible command), and do detaining powers honor qualifying claims when presented?',
    'Track documented cases of non-state forces adopting the required markers and compare subsequent treatment against declared criteria compliance; examine levee-en-masse and mass-reservist edge cases where the criteria are historically relaxed for state forces.',
    'Honored qualifying claims confirm the boundary is earnable and the coordination function genuine; systematically denied claims reveal a closed boundary maintained by enforcement alone — reclassification pressure toward pure extraction and a coalition-power finding for the organized payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_adoption_escape_route, empirical, 'Whether criteria-meeting is a real escape route or a formally open, practically closed door.').

omega_variable(
    limbo_detention_frame_gap,
    'Does observed detention practice — fighters held in a legal category that is neither protected-prisoner nor processed criminal — depart from this reading''s own coherent frame, under which excluded fighters are handled under domestic criminal law?',
    'Compare processing outcomes (formal trial rates, release timelines, notification and visiting access) for detainees channeled into criminal-process frames versus those held in undetermined status.',
    'Persistent limbo detention means realized treatment runs beyond what the reading''s stated frame licenses: effective extraction exceeds the authored value and the arrangement''s operation drifts toward enforced exclusion despite the endorsed principled frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limbo_detention_frame_gap, empirical, 'Gap between the reading''s stated criminal-handling frame and observed neither-nor detention practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_state_centric_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t1949, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t1959, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1959, 0.18).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t1959, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t1969, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1969, 0.22).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t1969, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t1977, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t1977, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t1987, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1987, 0.28).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t1987, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t2001, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t2011, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2011, 0.33).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t2011, observed).
narrative_ontology:measurement(geneva_state_centric_tr_t2025, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(geneva_state_centric_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(geneva_state_centric_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement_basis(geneva_state_centric_be_t1949, observed).
narrative_ontology:measurement(geneva_state_centric_be_t1959, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1959, 0.34).
narrative_ontology:measurement_basis(geneva_state_centric_be_t1959, observed).
narrative_ontology:measurement(geneva_state_centric_be_t1969, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1969, 0.38).
narrative_ontology:measurement_basis(geneva_state_centric_be_t1969, observed).
narrative_ontology:measurement(geneva_state_centric_be_t1977, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement_basis(geneva_state_centric_be_t1977, observed).
narrative_ontology:measurement(geneva_state_centric_be_t1987, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1987, 0.47).
narrative_ontology:measurement_basis(geneva_state_centric_be_t1987, observed).
narrative_ontology:measurement(geneva_state_centric_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(geneva_state_centric_be_t2001, observed).
narrative_ontology:measurement(geneva_state_centric_be_t2011, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2011, 0.54).
narrative_ontology:measurement_basis(geneva_state_centric_be_t2011, observed).
narrative_ontology:measurement(geneva_state_centric_be_t2025, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2025, 0.5).
narrative_ontology:measurement_basis(geneva_state_centric_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_state_centric_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement_basis(geneva_state_centric_su_t1949, observed).
narrative_ontology:measurement(geneva_state_centric_su_t1959, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1959, 0.32).
narrative_ontology:measurement_basis(geneva_state_centric_su_t1959, observed).
narrative_ontology:measurement(geneva_state_centric_su_t1969, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1969, 0.38).
narrative_ontology:measurement_basis(geneva_state_centric_su_t1969, observed).
narrative_ontology:measurement(geneva_state_centric_su_t1977, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1977, 0.44).
narrative_ontology:measurement_basis(geneva_state_centric_su_t1977, observed).
narrative_ontology:measurement(geneva_state_centric_su_t1987, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1987, 0.52).
narrative_ontology:measurement_basis(geneva_state_centric_su_t1987, observed).
narrative_ontology:measurement(geneva_state_centric_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement_basis(geneva_state_centric_su_t2001, observed).
narrative_ontology:measurement(geneva_state_centric_su_t2011, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2011, 0.66).
narrative_ontology:measurement_basis(geneva_state_centric_su_t2011, observed).
narrative_ontology:measurement(geneva_state_centric_su_t2025, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(geneva_state_centric_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the Geneva Conventions' protective scope' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — measuring scope by individual status criteria, by universal personhood, or by conflict-type scaling yields different victim sets and different epsilon over the same standing arrangement, so they are three files, not one story with a measurement parameter. This file (state_centric_reading) is the upstream anchor: its criteria-gate is the arrangement the other two readings contest, and state practice under this reading shifts the legitimacy conditions the siblings operate in. Universal_rights_reading authors the highest family epsilon for the shared referent; hybrid_proportionality_reading sits between. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
