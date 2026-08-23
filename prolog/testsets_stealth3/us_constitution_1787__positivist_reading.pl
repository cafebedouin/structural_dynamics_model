% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Positivist Reading of the U.S. Constitution: Meaning Is Text Plus Amendments, Courts Confined to Text
 *   domain: constitutional law/legal theory/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_1787 - the positivist reading: constitutional meaning is
 *   what the enacted text says plus whatever the Article V amendment process
 *   adds, and judicial interpretation is confined to that text. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest - the positivist arrangement itself - assessed
 *   by the reading's own lights, which openly concede that claims the text
 *   does not anchor must wait on supermajoritarian amendment and price that
 *   wait as the cost of keeping meaning in democratic hands. The claim and
 *   the metrics are independent authored facts: claimed_type records the
 *   structure I believe true (a genuine coordination core carrying a real
 *   asymmetric gating cost), while the metrics describe the arrangement's
 *   operation as the historical record shows it. KEY AGENTS (by structural
 *   relationship): - federal_judiciary: administering enforcer
 *   (institutional/constrained) - applies the text, declines extra-textual
 *   readings, forfeits the latitude a rival reading would grant -
 *   constitutional_amendment_coalitions: primary beneficiary
 *   (organized/constrained) - transient supermajorities holding the only
 *   legitimate pen - elected_branches: primary beneficiary
 *   (institutional/constrained) - durably receives the interpretive authority
 *   courts forgo - state_legislatures: secondary beneficiary
 *   (organized/constrained) - co-owners of the ratification gate,
 *   individually unable to move it - discrete_and_insular_minorities: primary
 *   target (powerless/trapped) - protection must arrive as text or as
 *   supermajority consent - pre_codification_rights_movements: secondary
 *   target (organized/constrained) - bear the amendment gate's full latency -
 *   territorial_residents: excluded voice (powerless/trapped) - bound by the
 *   mechanism, unrepresented in it - legal_academy: analytical observer
 *   (analytical/analytical) - maps the interpretive regimes and supplies
 *   their institutional carriers
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.62).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Positivist Reading of the U.S. Constitution: Meaning Is Text Plus Amendments, Courts Confined to Text").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional law/legal theory/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '8a5e83d3-d513-4292-bec4-75ed49516626').
narrative_ontology:cs_kernel_codification('8a5e83d3-d513-4292-bec4-75ed49516626', fixed_text).
narrative_ontology:cs_authority_grounding('8a5e83d3-d513-4292-bec4-75ed49516626', lineage).
narrative_ontology:cs_reading_relation('8a5e83d3-d513-4292-bec4-75ed49516626', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a5e83d3-d513-4292-bec4-75ed49516626', us_constitution_1787__living_reading, influences).
narrative_ontology:cs_axiom('8a5e83d3-d513-4292-bec4-75ed49516626', foundational, current_text_semantics_bind_judges).
narrative_ontology:cs_axiom_status(current_text_semantics_bind_judges, holdable).
narrative_ontology:cs_axiom_grounding('8a5e83d3-d513-4292-bec4-75ed49516626', current_text_semantics_bind_judges, conventional).
narrative_ontology:cs_axiom('8a5e83d3-d513-4292-bec4-75ed49516626', foundational, amendment_exclusive_meaning_change_channel).
narrative_ontology:cs_axiom_status(amendment_exclusive_meaning_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('8a5e83d3-d513-4292-bec4-75ed49516626', amendment_exclusive_meaning_change_channel, deontological).
narrative_ontology:cs_reference_frame('8a5e83d3-d513-4292-bec4-75ed49516626', enacted_text_plus_amendment_canon).
narrative_ontology:cs_drift_state('8a5e83d3-d513-4292-bec4-75ed49516626', contemporary_textualist_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a5e83d3-d513-4292-bec4-75ed49516626', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, constitutional_amendment_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, elected_branches).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, state_legislatures).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, discrete_and_insular_minorities).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, pre_codification_rights_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the arrangement day to day: decides what the enacted text permits, declines readings that outrun the words, and thereby enforces the discipline that meaning lives in the document. Bears the arrangement's cost in foregone latitude - questions the text does not answer get no judicial answer - and its method is policed externally by confirmation politics and internally by professional expectation. Exit means leaving the bench; a sitting judge cannot adopt a rival method without institutional consequence.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, federal_judiciary, payer).

% Transient supermajorities - two-thirds of each House of Congress plus three-quarters of state legislatures - that alone may add to or alter the text. While assembled they hold the only legitimate pen; when the coalition dissolves its members revert to ordinary actors bound by whatever text they wrote. The channel's exclusivity is what they collect; assembling it is rare and expensive.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_amendment_coalitions, beneficiary,
    organized, generational, constrained, national).

% Congress and the presidency keep the policy-making authority the arrangement withholds from courts: no rival interpreter stands above them, and questions the text leaves open return to them as politics rather than migrating into judicial discretion. They durably receive the interpretive authority the bench forgoes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, elected_branches, beneficiary,
    institutional, biographical, constrained, national).

% Fifty legislatures each hold one vote in the three-quarters ratification gate - a veto over any change and, jointly, the initiative to force one. Individually they can block but not make meaning; they are also bound by federal constitutional limits they did not choose, most extensively since the Reconstruction-era amendments nationalized rights enforcement.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, state_legislatures, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, state_legislatures, payer).

% Groups whose treatment turns on constitutional limits they cannot reach: their protection must be written into the text or won by persuading supermajorities, because the arrangement gives courts no mandate to protect what the words do not mention. Historically the wait ran generations - abolition and universal suffrage each required amendment, and the first followed a civil war. Leaving the jurisdiction is possible in principle and ruinous in practice for most members.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, discrete_and_insular_minorities, payer,
    powerless, biographical, trapped, national).

% Organized movements pressing claims the text does not yet contain - abolition before the Thirteenth Amendment, woman suffrage before the Nineteenth, and present-day movements around privacy, reproduction, or digital labor where the words are silent or hostile. They bear the full latency of the amendment gate: decades of persuasion, defeat, and retry, with no judicial shortcut available under this reading.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, pre_codification_rights_movements, payer,
    organized, biographical, constrained, national).

% Residents of the District of Columbia, Puerto Rico, Guam, the Virgin Islands, American Samoa, and the Northern Marianas live under the Constitution's limits but hold no voting member of Congress and no state legislature to ratify amendments. They are bound by a mechanism whose legitimacy story is democratic participation they are structurally denied; their objection never enters the room because the room is constituted by the very representation they lack.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, territorial_residents, excluded,
    powerless, biographical, trapped, regional).

% Constitutional scholars, law schools, and the journal literature: they map the interpretive regimes, referee method disputes in print and classroom, train the judges and staffers who carry methods into institutions, and supply the arguments confirmation committees draw on. An analytical seat - neither collecting nor paying - though their method allegiances shape which reading has institutional carriers.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, elected_branches).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes constitutional meaning in a publicly inspectable text and reserves its revision to one supermajoritarian procedure, so courts, officials, states, and citizens transact under a single stable referent and fundamental change requires breadth approximating consensus rather than winning five votes.
% TRANSFER_FUNCTION: Moves interpretive authority from courts to the enacted text and the amendment process; moves the cost of constitutional change onto those whose claims require amendment - they must assemble supermajorities - and moves the benefit of determinacy to every actor transacting under the rules.
% ABSENT_VOICES: Territorial residents and the disenfranchised of earlier epochs (enslaved people until 1865, women before 1920 in most of the country) are bound by the text yet excluded from both ratification and amendment. They would object that an arrangement legitimated as democratic routes all change through a franchise that historically excluded them; under this reading their objection has no institutional address short of the amendment gate itself.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight - if constitutional meaning decoupled from the enacted text and its amendment canon - courts would become the primary meaning-makers by default, amendment coalitions would lose their exclusive channel, and textually unanchored movements would redirect effort from supermajority-building to litigation. Branch relations would renegotiate: Congress would police courts it can no longer out-write, and the determinacy that markets, states, and officials transact under would degrade until a successor regime stabilized.
% FOUNDING_PROBLEM: Securing supreme law that outlives factions: the Articles period showed statutes and confederal rules rewritten at will by shifting legislative majorities. The 1787 design answered with a fixed text alterable only by a deliberately difficult supermajoritarian process, so that neither temporary majorities nor judicial officers could rewrite fundamental rules unilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: Anti-Federalist as well as Federalist writers attested the instability problem in 1787-88 while disputing the remedy; comparative constitutional scholarship finds formal amendment rules in the large majority of modern written constitutions, attesting the stability problem is general rather than a rationalization; and living-constitutionalist scholars - opponents of this reading - concede the stability problem is real while denying that amendment should be its only channel.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.62 because the arrangement's conceded cost is real and currently unbuffered: rights claims without textual anchoring face a gate whose release valve has produced one technical amendment since 1971, while the interpretive counter-revival re-tightens judicial confinement. Suppression (0.58) is enforcement of an interpretive discipline - confirmation litmus tests, scholarly sanction, court-majority policing of method - coercive within the profession though not carceral; it is authored raw and unscaled, per the structural-property rule. Theater (0.26) is low-moderate: textual citation is mostly doing real work under the revived method, with a residual performative share inherited from the mid-century pretense. Accessibility_collapse (0.30) is low because the kernel contest keeps rival readings fully alive - understanding this arrangement does not close off the alternatives, it sharpens the argument among them. Resistance (0.60) reflects sustained opposition from living-constitutionalist jurists and scholars and from movements that need judicial shortcuts the reading denies. The measurement series run on one shared seven-point grid (every tracked metric authored at every point, t0=1787 through t240=2027); the trajectory oscillates rather than drifting monotonically - extraction peaked when the gate failed catastrophically (emancipation required civil war), fell while the amendment valve demonstrably worked and while courts bypassed the gate altogether, and climbs again as the valve idles and the method consolidates. The oscillation is driven by two exogenous factors - amendment-channel viability and interpretive-method fashion - not by intermittent reinforcement; base_properties are measured at the interval's end phase (re-tightened gate, idle valve).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the amendment-coalition and elected-branch seats the arrangement is a coordination achievement they operate and profit from: determinacy plus exclusive democratic authorship. From the trapped, powerless payer seats the same structure is a gate they cannot open: protection arrives only as text or supermajority consent, and the supermajority threshold is calibrated precisely so that they, numerically, cannot meet it alone. Coalition power exists - the Reconstruction and suffrage amendments prove it - but historically required catastrophe or generations of organization, which is the cost, not the absence, of the path. The judiciary seat is genuinely dual: it administers the discipline and bears it, giving up the latitude a sibling reading would grant, so its computed position should sit nearer symmetry than either pole. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (amendment coalitions, elected branches, state legislatures) derive low directionality - the arrangement subsidizes them with authorship and retained authority. Payers (discrete minorities, pre-codification movements) derive high directionality - trapped exit and powerless-or-overmatched power place them near the full-target end, and national scope amplifies the difficulty of verifying the gate's fairness. The judiciary sits near the middle: it runs the arrangement and is run by it, which the secondary_role declares and the derivation reads. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate the seats, and the one genuinely dual-positioned agent is handled by its declared secondary role rather than by an override. Receipt: the extraction's gains demonstrably land on the elected_branches, which durably receive the interpretive authority courts forgo - amendment coalitions collect only episodically, so the receipt seat is the durable one. Fixing is cheap in the binary cost-class sense: interpretive regimes have flipped twice within living memory (the New Deal transformation, the textualist consolidation) without formal amendment, so the cost of fixing, while real, is not prohibitive relative to the benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - supreme law that outlives factions - is live, so this is not a resolved mandate kept alive by inertia; the arrangement still performs its designed function wherever the text answers and the valve works. The classification guards against two mislabels. Against pure-rope mislabeling: the gating cost is not coordination overhead but asymmetric incidence - the supermajority threshold is passable by majorities and structurally impassable by the minorities it gates, which is why victims are declared and active enforcement is declared. Against snare mislabeling: the coordination function is genuine and primary - determinacy, inspectability, and democratic ownership of meaning are real goods delivered to every transacting actor, and the exit it blocks (informal judicial evolution) is blocked by argument and method, not by suppressing the claimants' existence. The live risk is drift: if the amendment valve stays idle while judicial confinement consolidates, the arrangement approaches a gate without a release valve and the payer seats' computed extraction hardens - the measurement series is built to catch exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_location,
    'Across the three readings of us_constitution_1787, is the live disagreement located in the SOURCE of meaning (current text vs ratification-era semantics vs evolving society) or in the LOCUS of authorized change (amendment gate vs judicial judgment)?',
    'Cross-reading comparison holding the referent fixed: compute each sibling reading''s victim set and epsilon over the same standing arrangement and locate where the victim sets actually diverge.',
    'If the contest is chiefly about the change locus, this reading and the living_reading converge more than their labels suggest (both can honor amendment as a channel), and the sharp victim-set differences attributed to textual confinement shrink; if it is about the meaning source, the originalist sibling diverges less from this one than commonly assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_location, conceptual, 'Where the kernel contest is actually located across the sibling readings.').

omega_variable(
    amendment_channel_viability,
    'Is Article V a live democratic mechanism or effectively closed - and does the positivist arrangement therefore gate rights claims without a working release valve?',
    'Amendment-frequency and state-legislature application data across eras; comparison of rights-won-by-amendment rates in the progressive-era wave against the post-1971 idle period.',
    'If the channel is dead, the arrangement''s extraction concentrates sharply on textually unanchored claimants and the payer seats'' computed extraction hardens toward the snare side; if revivable, the gating is coordination-priced and the rope side of the ledger strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_channel_viability, empirical, 'Whether the amendment gate has a functioning release valve.').

omega_variable(
    minority_protection_substitutes,
    'Do non-judicial substitutes - statutory overlays, federalism, electoral coalitions - adequately protect claims the text does not anchor, or does the arrangement leave them systematically exposed?',
    'Compare trajectories of textually anchored versus unanchored rights claims across the amendment-active and amendment-idle eras, controlling for political salience.',
    'If substitutes suffice, the victim extraction attributed to judicial confinement is overstated and the coordination reading gains; if not, the payer seats'' computed extraction stands and the asymmetric half of the ledger is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_substitutes, empirical, 'Whether the arrangement''s gating costs are offset outside the courts.').

omega_variable(
    interpretive_discipline_internalization,
    'Is the enforcement of text-bound judging structural (confirmation politics, institutional sanction) or internalized (the professional self-conception of the judge as a restrained applier of the words)?',
    'Observe sitting judges'' interpretive behavior after the external pressure that selected them recedes - life tenure decouples daily output from sponsoring coalitions; divergence patterns separate imposed from absorbed discipline.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists through personnel turnover; if mostly structural, enforcement tracks confirmation politics and flips with appointments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_discipline_internalization, empirical, 'Structural versus internalized enforcement of the textual discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ucpr_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ucpr_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(ucpr_tr_t80, us_constitution_1787__positivist_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(ucpr_tr_t120, us_constitution_1787__positivist_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement(ucpr_tr_t160, us_constitution_1787__positivist_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(ucpr_tr_t200, us_constitution_1787__positivist_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(ucpr_tr_t240, us_constitution_1787__positivist_reading, theater_ratio, 240, 0.26).

% Extraction over time
narrative_ontology:measurement(ucpr_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(ucpr_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(ucpr_be_t80, us_constitution_1787__positivist_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(ucpr_be_t120, us_constitution_1787__positivist_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(ucpr_be_t160, us_constitution_1787__positivist_reading, base_extractiveness, 160, 0.46).
narrative_ontology:measurement(ucpr_be_t200, us_constitution_1787__positivist_reading, base_extractiveness, 200, 0.54).
narrative_ontology:measurement(ucpr_be_t240, us_constitution_1787__positivist_reading, base_extractiveness, 240, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ucpr_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ucpr_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(ucpr_su_t80, us_constitution_1787__positivist_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(ucpr_su_t120, us_constitution_1787__positivist_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(ucpr_su_t160, us_constitution_1787__positivist_reading, suppression_requirement, 160, 0.34).
narrative_ontology:measurement(ucpr_su_t200, us_constitution_1787__positivist_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(ucpr_su_t240, us_constitution_1787__positivist_reading, suppression_requirement, 240, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Constitution constrains government' conflates three structurally distinct constraints that share a text but differ in victim set, epsilon, and failure mode: the originalist reading tethers judges to ratification-era semantics; the living reading licenses society-driven evolution; the positivist reading confines judges to the current text and routes all change through the amendment gate. They are modeled as a linked family, not one story with a measurement parameter: each carries its own stable epsilon over the same standing arrangement, and the coupling runs through shared institutional carriers (the judiciary and the confirmation pipeline), so a purity shift in any one propagates to the others. This file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
