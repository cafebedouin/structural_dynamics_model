% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Consent-Bounded Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   rome_statute_jurisdiction: the sovereigntist reading, under which the
 *   Rome Statute's jurisdictional regime is a conditional framework requiring
 *   strict sovereign consent, leaving non-party nationals immune short of
 *   Security Council referral, keeping national courts primary, and casting
 *   complementarity as deference rather than override. The universalist and
 *   hybrid-complementarity readings are OTHER constraints (other files) and
 *   are not averaged into this one. Epsilon's referent is the standing
 *   arrangement under contest: the consent-conditioned jurisdictional regime
 *   as it actually operates, including Security Council referral practice and
 *   the bilateral-immunity-agreement network, assessed by the sovereigntist
 *   reading's own lights, not by the universalist alternative it rejects. The
 *   claimed type (tangled_rope) and the metric values were authored
 *   independently: the claim states what this reading takes the structure to
 *   be; the metrics describe how the arrangement has actually performed
 *   across 1998-2025. KEY AGENTS (by structural relationship):
 *   nonparty_great_powers: primary beneficiary (institutional/arbitrage) -
 *   collects immunity without obligation; permanent_five_members: gatekeeping
 *   beneficiary (institutional/arbitrage) - controls the referral channel;
 *   ratifying_state_executives: dual-positioned (organized/constrained) -
 *   retains primacy, pays cooperation costs;
 *   nationals_of_weak_ratifying_states: primary target (powerless/trapped) -
 *   bears asymmetric exposure; atrocity_victims_in_nonparty_states: excluded
 *   cost-bearer (powerless/trapped) - forum denied by the consent rule;
 *   icc_officials: agenda-setter inside consent bounds
 *   (institutional/constrained); civil_society_accountability_coalition:
 *   excluded advocate (organized/mobile); treaty_law_scholars: analytical
 *   observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.44).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.52).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Consent-Bounded Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '8024b6ec-38f7-41af-b8dd-9436615df747').
narrative_ontology:cs_kernel_codification('8024b6ec-38f7-41af-b8dd-9436615df747', fixed_text).
narrative_ontology:cs_authority_grounding('8024b6ec-38f7-41af-b8dd-9436615df747', lineage).
narrative_ontology:cs_interpretation_layer_present('8024b6ec-38f7-41af-b8dd-9436615df747').
narrative_ontology:cs_reading_relation('8024b6ec-38f7-41af-b8dd-9436615df747', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('8024b6ec-38f7-41af-b8dd-9436615df747', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('8024b6ec-38f7-41af-b8dd-9436615df747', foundational, jurisdiction_requires_express_consent).
narrative_ontology:cs_axiom_status(jurisdiction_requires_express_consent, holdable).
narrative_ontology:cs_axiom_grounding('8024b6ec-38f7-41af-b8dd-9436615df747', jurisdiction_requires_express_consent, conventional).
narrative_ontology:cs_axiom('8024b6ec-38f7-41af-b8dd-9436615df747', foundational, national_courts_hold_primary_jurisdiction).
narrative_ontology:cs_axiom_status(national_courts_hold_primary_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('8024b6ec-38f7-41af-b8dd-9436615df747', national_courts_hold_primary_jurisdiction, deontological).
narrative_ontology:cs_reference_frame('8024b6ec-38f7-41af-b8dd-9436615df747', strict_consent_treaty_framework).
narrative_ontology:cs_drift_state('8024b6ec-38f7-41af-b8dd-9436615df747', contemporary_post_afghanistan_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8024b6ec-38f7-41af-b8dd-9436615df747', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, permanent_five_members).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, ratifying_state_executives).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_nonparty_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, nationals_of_weak_ratifying_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, ratifying_state_executives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remain outside the treaty while conducting diplomacy, trade, and military operations worldwide. Their nationals are structurally unreachable by the Court except through a Security Council referral their own veto blocks. Several have negotiated bilateral agreements with dozens of states barring surrender of their personnel, and one has imposed sanctions and travel bans on Court officials. Exit is trivial: they never entered.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Hold veto power over Security Council referrals, the only channel that reaches non-consenting territory. Three of the five sit outside the treaty entirely; two are parties. Collectively they decide which situations ever reach the Court's docket from non-member states, and none can in practice be referred against itself.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, permanent_five_members, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, permanent_five_members, agenda_setter).

% Ratified the treaty and gained a standing backstop: their courts stay primary, the international court steps in only upon unwillingness or inability, and their governments bank credibility from membership. They also fund the court, answer cooperation requests, and absorb diplomatic friction when warrants touch allies. Withdrawal exists and a few states have used it, but it carries reputational and aid-relationship costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, ratifying_state_executives, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, ratifying_state_executives, payer).

% Soldiers, officials, and militia commanders from smaller member states appear before the Court while great-power counterparts do not; their exposure tracks a treaty choice their government made without their individual consent. Arrest depends on their own government's cooperation, which historically arrives more readily from weak states than strong ones.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, nationals_of_weak_ratifying_states, payer,
    powerless, biographical, trapped, national).

% Survivors of atrocities in states that never joined the treaty have no path to this Court unless the Security Council acts, which the veto makes rare. Their access to justice was priced by a consent rule negotiated without them; where their own government is the perpetrator, the consent rule leaves them with no international forum at all.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_nonparty_states, excluded,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, atrocity_victims_in_nonparty_states, payer).

% Prosecutors and judges select situations and issue warrants inside the boundaries the consent architecture draws. Every investigation depends on state cooperation for arrests, evidence, and access; several warrants have gone unexecuted for years. Budget and political survival run through the assembly of member states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_officials, agenda_setter,
    institutional, generational, constrained, global).

% NGO coalitions campaigned for the court's creation and now press for wider reach: universal ratification, Security Council referral reform, and national universal-jurisdiction statutes. They are heard in assembly corridors but hold no vote and no seat in the consent bargain; their preferred routes run outside the treaty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, civil_society_accountability_coalition, excluded,
    organized, generational, mobile, global).

% International lawyers and jurists debate what the statute's jurisdiction articles mean, track the case law against the negotiating record, and publish the doctrinal analyses that all three readings of the jurisdiction kernel cite. They collect no rents and bear no exposure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, treaty_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of prosecuting genocide, war crimes, and crimes against humanity without triggering a universal-jurisdiction free-for-all: states pre-agree the exact conditions under which an international court may act, national courts keep primacy, and each state's exposure is predictable and bounded by its own consent.
% TRANSFER_FUNCTION: Moves adjudicative authority from national governments to an international court only along consented channels; moves cooperation burdens (arrests, evidence, funding) onto ratifying states; and moves effective impunity-shielding to non-party great powers whose nationals stay outside the perimeter.
% ABSENT_VOICES: Atrocity victims in non-consenting states and the civil-society accountability coalition would object that strict consent converts their suffering into a bargaining chip held by the very governments most likely to shelter perpetrators. They are outside the room because the framework's admission ticket is state consent, which abusive governments withhold; the consent rule was negotiated among states, not with the people whose protection it prices.
% DISAPPEARANCE_RATIONALE: If the consent-conditioning vanished overnight, the framework would either become a universal-mandate court, provoking immediate great-power retaliation, funding collapse, and mass withdrawal, or dissolve into ad hoc tribunals and scattered national universal-jurisdiction proceedings. Cooperation decisions, Article 98 networks, and assembly politics would all reorganize around whichever replacement emerged.
% FOUNDING_PROBLEM: Post-Nuremberg and post-Cold War impunity: ad hoc tribunals for Yugoslavia and Rwanda proved international criminal justice possible but slow, expensive, and selective, so states sought a permanent court that would not require a new treaty for each crisis, while insisting it never override national courts or bind states that had not agreed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN General Assembly mandates that produced the statute, the 2020 Independent Expert Review of the Court, Amnesty International and Human Rights Watch documentation of ongoing impunity gaps, and the international criminal law scholarship recording that Syria, Myanmar, and Ethiopia situations remained unreachable. Great-power non-parties implicitly corroborate the problem's liveness while disputing the remedy's reach.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.44 at interval end) because the consent condition genuinely bounds what the Court can take: participating states bought predictability and retained primacy, and the Court's direct coercive footprint is thin. But the same architecture distributes its costs unevenly: great-power nationals sit outside the perimeter, weak-state nationals sit inside it, and non-party atrocity victims are priced out entirely, which lifts epsilon well above rope floor. Suppression (0.52) is a raw structural property, unscaled by power or scope: the framework actively suppresses alternative accountability routes (universal-jurisdiction statutes narrowed under great-power pressure, bilateral agreements contractually blocking alternative surrender channels, sanctions punishing outreach beyond the consent bounds) even though joining itself was voluntary. Theater ratio (0.25) reflects real convictions alongside accumulating ritual: annual assemblies, complementarity monitoring, and ratification campaigns that substitute motion for reach. Accessibility collapse is low-moderate (0.38) because alternatives persist and remain usable: ad hoc and hybrid tribunals, national universal-jurisdiction prosecutions, and evidence mechanisms outside the treaty. Resistance is substantial (0.60): signature withdrawals, state-party withdrawals, sanctions on Court personnel, and organized non-cooperation strategies. The temporal series share one grid (1998, 2002, 2007, 2012, 2017, 2020, 2023, 2025) with every tracked metric authored at every point. All three series rise to a 2020 peak and partially recede: the rise tracks the framework's activation and the sharpening of its asymmetries (African situation concentration, the Afghanistan jurisdiction fight, sanctions on the prosecutor); the recede tracks the Ukraine-era realignment, in which the Court became useful to aligned states and enforcement pressure partially relaxed. The oscillation is driven by great-power reaction cycles rather than intermittent reinforcement: each assertion of reach provoked counter-pressure, which then eased as geopolitical incentives shifted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same text. From the nonparty_great_powers and permanent_five_members positions, the consent condition is the framework's crowning virtue: it is what keeps their nationals untouchable and their discretion intact, and they defend it with vetoes, bilateral agreements, and sanctions. From the nationals_of_weak_ratifying_states and atrocity_victims_in_nonparty_states positions, the identical clause is the wall: it fixes their exposure and locks them out of the forum. The icc_officials seat experiences the constraint as both mandate and cage: the consent bounds give the Court its legitimacy while capping its reach, and every expansion attempt converts legitimacy capital into resistance. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nonparty_great_powers sit nearest the beneficiary pole (d near 0.0): the consent rule subsidizes them with immunity they paid nothing to obtain, and their arbitrage-grade exit means no effective extraction touches them. Permanent_five_members collect gatekeeping rents over the referral channel with the same arbitrage position. Ratifying_state_executives derive real benefit (primary-authority retention, credibility, a backstop against their own worst actors going unpunished) while paying funding and cooperation costs, placing them modestly off the beneficiary pole. Nationals_of_weak_ratifying_states sit near the full-target pole (d near 1.0): they bear the exposure side of an asymmetry they cannot individually exit, and identity-of-nationality traps them regardless of personal conduct. Atrocity_victims_in_nonparty_states are extracted from in the specific currency of forum denial: the constraint's operation consumes their claim to international justice and transfers that value to the sovereignty protection others enjoy. Icc_officials sit near symmetric: they administer the structure and bear its enforcement dependence without collecting its gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabelings. Reading the arrangement as a snare would erase the genuine coordination function: states really did solve a collective-action problem, exposure really is consent-bounded, national courts really do stay primary, and the founding problem (impunity gaps) is still live, so nothing here persists past its mandate. Reading it as a pure rope would erase the measurable asymmetry: the same consent clause that legitimizes the Court for parties shields non-party great powers, the referral channel concentrates trigger power in five hands, and the burden of cooperation falls hardest on the weakest participants, which is extraction running through the coordination structure rather than beside it. Tangled_rope holds both facts. No mandatrophy is declared: the founding problem remains live and the arrangement still performs its transitional-none, steady-state function of bounded adjudication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_rome_statute_kernel,
    'This constraint is one reading (sovereigntist_reading) of the kernel rome_statute_jurisdiction. What would the sibling readings change structurally, and where exactly does the disagreement bite?',
    'Comparative classification across the three linked reading-files: the universalist reading relocates non-party officials into the target set and raises extractiveness and resistance; the hybrid reading replaces party-status with willingness-and-ability as the exposure criterion. The disagreement is located in whether consent is the exclusive legitimacy condition for jurisdiction.',
    'If the universalist reading prevails institutionally, this file''s beneficiary structure inverts (non-party great powers become targets, not collectors) and the type migrates toward snare-flavored profiles; if the hybrid reading prevails, the victim set fragments by state capacity and the sharp great-power asymmetry dissolves into a graded one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_rome_statute_kernel, conceptual, 'Committer structure: this file is the sovereigntist member of a three-reading kernel family; sibling files instantiate the other readings.').

omega_variable(
    unsc_referral_consistency_ambiguity,
    'Do Security Council referrals over non-party territories (Sudan, Libya) violate or fulfill the strict-consent premise this reading rests on?',
    'Doctrinal analysis of whether Chapter VII obligations, consented to through UN membership, transmit consent sufficient for referral-based jurisdiction over non-party nationals, versus treating such referrals as pacta tertiis breaches inside the reading''s own logic.',
    'If referrals over non-parties are breaches, the standing arrangement already violates its own consent logic and epsilon rises; if they are consent-derived through the Charter, the framework remains internally coherent and epsilon stays at the authored level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_consistency_ambiguity, conceptual, 'Internal ambiguity in the sovereigntist reading about the Security Council referral pathway.').

omega_variable(
    article98_network_suppression_extent,
    'How much do the bilateral immunity agreements actually extend great-power immunity into party territory, and how much do they suppress party states'' alternative surrender routes?',
    'Count and audit of executed bilateral agreements, litigation over surrender requests, and assembly-of-states-parties responses; compare party behavior in states with and without agreements.',
    'An extensive, enforced agreement network raises measured suppression and widens the extraction asymmetry; marginal or dormant usage lowers both and moves the profile toward the rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article98_network_suppression_extent, empirical, 'Empirical extent of the bilateral immunity network''s effect on the consent framework''s operation.').

omega_variable(
    complementarity_deference_vs_override,
    'Does complementarity operate in practice as deference to national proceedings (this reading''s claim) or as override through successful admissibility challenges?',
    'Track admissibility rulings and their outcomes: if challenges routinely fail where states genuinely investigate, complementarity is deferential; if the Court frequently displaces national proceedings, it functions as override.',
    'If complementarity overrides, this reading''s descriptive core weakens and the hybrid sibling reading gains support; if it defers, the sovereigntist characterization is confirmed and the constraint''s coordination function is stronger than the metrics alone suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_deference_vs_override, empirical, 'Whether the complementarity mechanism defers to or overrides national courts in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.13).
narrative_ontology:measurement(rome_tr_t2007, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2007, 0.17).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2012, 0.21).
narrative_ontology:measurement(rome_tr_t2017, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(rome_tr_t2023, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2023, 0.27).
narrative_ontology:measurement(rome_tr_t2025, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.26).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.31).
narrative_ontology:measurement(rome_be_t2007, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2007, 0.36).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2012, 0.41).
narrative_ontology:measurement(rome_be_t2017, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(rome_be_t2023, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2023, 0.47).
narrative_ontology:measurement(rome_be_t2025, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2025, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.33).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.4).
narrative_ontology:measurement(rome_su_t2007, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2007, 0.46).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2012, 0.51).
narrative_ontology:measurement(rome_su_t2017, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2017, 0.57).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(rome_su_t2023, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2023, 0.56).
narrative_ontology:measurement(rome_su_t2025, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Rome Statute jurisdiction' decomposes into three structurally distinct constraints, one per reading of the shared kernel. The sovereigntist reading (this file) authors epsilon against the consent-bounded arrangement and yields a tangled_rope profile with great powers as principal gain-collectors. The universalist reading authors epsilon against a consent-transcending mandate, adding non-party officials to the target set and raising both extractiveness and resistance. The hybrid reading authors epsilon against a complementarity-balanced arrangement, splitting exposure by willingness and ability. The statutory text is upstream of all three; each sibling file links the other two via network.affects_constraints so contamination and drift propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
