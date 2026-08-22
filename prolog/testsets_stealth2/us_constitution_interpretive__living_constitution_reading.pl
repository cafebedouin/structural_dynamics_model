% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living-Constitution Reading of U.S. Constitutional Interpretation
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the American practice of constitutional interpretation under a written,
 *   amendable, centuries-old charter; this file authors the
 *   living-constitution reading: the arrangement under which constitutional
 *   meaning legitimately evolves with societal values and interpretive
 *   authority derives from the judiciary's reasoned adaptation to
 *   contemporary conditions. Under that reading the standing arrangement
 *   concentrates interpretive authority in the federal courts, expands
 *   federal power through evolving commerce and implied-powers doctrine, and
 *   recognizes unenumerated rights (privacy, dignity, equality) the ratified
 *   text does not name. Epsilon is authored for THAT standing arrangement as
 *   the living-constitution reading itself assesses it — the residue of
 *   coercion, override, and authority accumulation left after crediting the
 *   adaptations the reading regards as legitimate. The sibling readings are
 *   separate constraints with separate files: the originalist reading
 *   (meaning fixed at ratification) would restructure the beneficiary and
 *   victim sets almost completely — yesterday's claimants become tomorrow's
 *   targets — and the popular-constitutionalist reading relocates authority
 *   from the bench to political struggle. Per the epsilon-invariance
 *   principle this file does not average across readings; the committer
 *   structure is carried in the omega variables and the cs_structure block.
 *   Claim and metrics are independent authored facts: the claim states the
 *   hybrid structure believed true of this reading's arrangement; the metrics
 *   describe its observed operation.
 *
 * KEY AGENTS:
 *   - - federal_judiciary: Agenda setter (institutional/arbitrage) — decides which meanings evolve, entrenches results through precedent, accumulates interpretive authority
 *   - - civil_rights_expansion_claimants: Primary beneficiary (organized/constrained) — holds protections that exist only through evolved meaning
 *   - - reproductive_autonomy_advocates: Beneficiary turned contested (organized/constrained) — gained then partially lost federal protection as interpretive coalitions shifted
 *   - - lgbtq_rights_claimants: Primary beneficiary (organized/constrained) — dignity and equality holdings rest on adaptive reasoning
 *   - - federal_legislative_and_executive_branches: Dual-positioned (institutional/constrained) — gains commerce and implied-power reach, pays when statutes fall to judicial review
 *   - - state_governments: Primary target (institutional/trapped) — bears incorporation, preemption, and enforcement of evolved doctrine
 *   - - original_meaning_textualists: Target with locked exit (organized/identity_locked) — method delegitimated within the dominant regime; responds by capturing appointments rather than exiting
 *   - - democratic_majorities: Diffuse target (moderate/trapped) — enactments invalidated without consent; recourse limited to amendment or appointment turnover
 *   - - popular_constitutionalist_movements: Excluded voice (organized/constrained) — popular authorship of meaning treated as illegitimate between amendments
 *   - - constitutional_theory_academy: Analytical observer (analytical/analytical) — maps the method dispute and the gap between announced methods and outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living-Constitution Reading of U.S. Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '3acd6381-3856-40b2-bc77-410d70e5c40e').
narrative_ontology:cs_kernel_codification('3acd6381-3856-40b2-bc77-410d70e5c40e', fixed_text).
narrative_ontology:cs_authority_grounding('3acd6381-3856-40b2-bc77-410d70e5c40e', expertise).
narrative_ontology:cs_interpretation_layer_present('3acd6381-3856-40b2-bc77-410d70e5c40e').
narrative_ontology:cs_reading_relation('3acd6381-3856-40b2-bc77-410d70e5c40e', us_constitution_interpretive__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3acd6381-3856-40b2-bc77-410d70e5c40e', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('3acd6381-3856-40b2-bc77-410d70e5c40e', foundational, constitution_enacts_enduring_principles_not_fixed_applications).
narrative_ontology:cs_axiom_status(constitution_enacts_enduring_principles_not_fixed_applications, holdable).
narrative_ontology:cs_axiom_grounding('3acd6381-3856-40b2-bc77-410d70e5c40e', constitution_enacts_enduring_principles_not_fixed_applications, deontological).
narrative_ontology:cs_axiom('3acd6381-3856-40b2-bc77-410d70e5c40e', foundational, reasoned_judicial_adaptation_is_authoritative_channel).
narrative_ontology:cs_axiom_status(reasoned_judicial_adaptation_is_authoritative_channel, holdable).
narrative_ontology:cs_axiom_grounding('3acd6381-3856-40b2-bc77-410d70e5c40e', reasoned_judicial_adaptation_is_authoritative_channel, instrumental).
narrative_ontology:cs_reference_frame('3acd6381-3856-40b2-bc77-410d70e5c40e', reasoned_adaptive_interpretation_framework).
narrative_ontology:cs_drift_state('3acd6381-3856-40b2-bc77-410d70e5c40e', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3acd6381-3856-40b2-bc77-410d70e5c40e', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_legislative_and_executive_branches).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, state_governments).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, democratic_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, federal_legislative_and_executive_branches).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, substantive_due_process_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, incorporation_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, commerce_clause_expansion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what the Constitution requires, including whether meanings attached to its clauses still bind. Selects which disputed questions to hear through certiorari discretion, writes the opinions that relocate doctrine, and entrenches results through precedent. Its membership turns over through appointment politics, so the direction of adaptation shifts with each presidential term. Every successful adaptive ruling enlarges the set of controversies on which its word is final.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Racial-equality litigants, criminal defendants, and voting-rights claimants whose principal protections — equal-protection scrutiny, incorporation of the Bill of Rights against the states, federal anti-discrimination enforcement — were built by reading guarantees past their ratification-era applications. They reach these protections almost exclusively through federal litigation; most state political processes offer them less. Leaving the judicial channel would mean forfeiting the protections.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    organized, generational, constrained, national).

% Organizations and patients who relied on a federal privacy right built through twentieth-century due-process interpretation. When that line of doctrine was overruled, their protection reverted to state-by-state politics — the practical demonstration of how much of their position rested on continued adaptive majorities on the bench rather than on text.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% Claimants whose marriage recognition, decriminalization, and anti-discrimination protections rest on dignity and equality holdings the ratified text does not mention. Their position tracks the interpretive coalition on the bench rather than any enumerated clause.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Congress and the executive drew regulatory reach from broad readings of the commerce power and implied powers — the New Deal settlement, civil-rights enforcement authority, the administrative state. They also see statutes invalidated on substantive grounds and find policy content dictated by doctrine. Their lever over the arrangement is appointment politics, exercised slowly across terms.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_legislative_and_executive_branches, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__living_constitution_reading, federal_legislative_and_executive_branches, payer).

% States police health, safety, morality, and elections under their own constitutions, yet face incorporation of federal rights against their institutions, preemption under expanded federal commerce power, and federal enforcement of desegregation and voting rights. They cannot leave the union or decline the Supreme Court's appellate jurisdiction; their resistance runs through litigation, coordinated statutes, and pressure on appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, state_governments, payer,
    institutional, generational, trapped, regional).

% Scholars, movement lawyers, and judges committed to ratification-era public meaning as the measure of constitutional text. Inside the courtroom-centered adaptive regime their method long registered as dissent; their careers, networks, journals, and clerkship pipelines are built around the rival method. Rather than exit, they pursued capture of the appointing process, which succeeded enough after the 2010s to move several doctrines back toward historical tests.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, generational, identity_locked, national).

% Electoral coalitions whose enactments — labor protections in earlier eras, abortion regulation, gun regulation more recently — are invalidated by judicial rulings issued over their objection. Their remedies are the slowest available: a supermajority amendment process never successfully used to reverse a rights ruling, or waiting for appointments to change the bench.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, democratic_majorities, payer,
    moderate, biographical, trapped, national).

% Movements and theorists who hold that constitutional meaning is made in political struggle — reconstruction, the labor era, the civil-rights era — rather than conferred by judicial elaboration alone. The courtroom-centered arrangement channels constitutional energy into litigation and confirmation fights and treats popular authorship between amendments as extra-legal, keeping these voices at the edge of the conversation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, popular_constitutionalist_movements, excluded,
    organized, generational, constrained, national).

% Law faculties and theorists who map the methodological dispute, audit the gap between announced methods and decided cases, and supply the arguments both camps deploy. They decide nothing and collect nothing; their product is the record the other seats argue over.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_theory_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single authoritative constitutional framework operating across radically changed conditions without requiring an Article V amendment for each change: one continuous legal order, stable precedent, and a settled institutional channel for resolving what the supreme law requires.
% TRANSFER_FUNCTION: Moves interpretive authority over contested moral and structural questions from state institutions, legislatures, and electoral processes to the federal judiciary; imposes binding compliance obligations on losing litigants and states; distributes recognized rights and federal regulatory reach according to doctrinal outcomes.
% ABSENT_VOICES: Popular-constitutionalist movements claiming direct authorship of meaning sit at the edge of the courtroom-centered conversation; state legislators whose bills are preempted receive the decision as an order rather than a negotiation; ordinary voters appear only as an appointment electorate. All three would object to judicial monopoly over meaning and are present mainly as objects of rulings.
% DISAPPEARANCE_RATIONALE: If adaptive interpretation ceased overnight, every doctrine resting on evolved meaning — the incorporated Bill of Rights, equal-protection scrutiny, the New Deal commerce settlement, the substantive-due-process lines — would lose its warrant simultaneously; rights recognized only through interpretation would revert to state politics; the federal-state balance would snap back toward pre-twentieth-century limits; and a century of precedent would become open to relitigation.
% FOUNDING_PROBLEM: An eighteenth-century charter of few words had to govern a continental industrial democracy: the text underdetermined modern questions (national economic regulation, racial caste, new technologies), and the amendment process proved too heavy to close the gap case by case.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal historians across methodological camps document the New Deal constitutional crisis and the amendment bottleneck; originalist scholars concede the text underdetermines modern disputes even while rejecting adaptive remedies; the observable record — twenty-seven amendments, none reversing a judicial rights ruling — attests the gap the arrangement addresses. No camp denies the founding problem; they dispute the authorized response.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: substantial but bounded — the arrangement transfers binding authority from states, legislatures, and majorities to the bench, yet the same channel periodically reverses direction with appointments, and much of what it transfers is rights protection its targets also invoke elsewhere. Suppression 0.52: alternatives are not abolished but made costly — amendment is nearly unreachable, exit from judicial jurisdiction is unavailable, and rival methods compete mainly by capturing appointments. Theater 0.30: opinion-writing performs neutrality while outcome-tracking is well documented, but the reasoning does real coordinative work. Accessibility collapse 0.45: alternatives (amendment, appointment turnover, state resistance, popular constitutional politics) remain visible and partly usable. Resistance 0.65: sustained — an organized rival method, court-curbing proposals, episodic state defiance, and appointment warfare. All three series share one time grid (points 0-100 in steps of 20); the trajectories show the expansion/backlash cycle: rapid adaptive expansion mid-interval, entrenchment friction, and partial retrenchment at the end as the rival method captured the appointing pipeline. The oscillation is driven by appointment politics rather than by the arrangement's internal logic; the scalar base_properties values reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the bench, the arrangement is the legitimate exercise of reasoned elaboration — authority earned by adapting principle. From the state and majority seats, the same structure operates as rule without consent: orders issued over objection, enforced by a tribunal they cannot exit or quickly staff. From the claimant seats it is the only working channel of protection. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: the three claimant classes (their protections exist only through evolved meaning) and the federal political branches (regulatory reach from expanded commerce and implied powers). Victims declared: state governments (incorporation, preemption, enforcement), original-meaning textualists (method delegitimated, exit locked by professional identity), and democratic majorities (enactments invalidated over objection). The derivation places claimants near the beneficiary end (low d), states and textualists near the target end (high d), and majorities high but diffuse. The federal political branches are deliberately dual-positioned (beneficiary with secondary_role payer): they gain reach and lose statutes to the same tribunal. No directionality override is used: overrides key on the power atom, and this story's institutional seats point in opposite directions (bench near beneficiary, states near target), so a power-atom override would misapply across seats; the dual position is carried structurally instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so the mandate has not outlived its function and no mandatrophy resolution is declared. Classification discipline matters here in both directions: naming the genuine coordination function (continuous legal order under changed conditions) blocks a pure-extraction reading that would erase the civil-rights gains the arrangement delivered; naming the victims and the concentrated receiver of authority blocks a pure-coordination reading that would erase the counter-majoritarian costs borne by states and overridden majorities. The arrangement is both things at once, held in place by active enforcement — the hybrid signature. It is not transitional (no sunset anyone proposes), and its function is not atrophied (the receiver of authority is concentrated and the stakes are live), which distinguishes it from piton and scaffold placements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does this classification describe the living-constitution reading specifically, and how would the sibling readings restructure the beneficiary and victim sets?',
    'Generate the sibling stories (originalist_reading, popular_constitutionalism_reading) with their own epsilon, beneficiaries, and victims, then compare structural deltas across the family.',
    'If the originalist reading became the operative arrangement, the current beneficiary classes (reproductive autonomy, LGBTQ+ claimants) would migrate into the target set and the states'' burden would lighten; the epsilon referent stays the standing arrangement, but the reading-index flips the assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the classification within the interpretive-kernel family.').

omega_variable(
    counter_majoritarian_valence,
    'Is judicial override of electoral majorities a transfer away from their holders (rule without consent) or a protection those holders'' members individually invoke (rights enforcement)?',
    'Normative theory choice, informed by empirical work on the welfare effects of overridden enactments and on whether the overridden policies would have persisted absent review.',
    'Reading override as disenfranchisement raises effective extraction toward the pure-transfer end; reading it as rights enforcement lowers it toward coordination cost — the same facts support a snare-drift or a rope verdict depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_valence, preference, 'Valence ambiguity of counter-majoritarian override.').

omega_variable(
    dobbs_oscillation_or_reversal,
    'Is the terminal retrenchment in the measurement series (overruling of the federal privacy line, migration to historical tests) a durable reversal of the adaptive regime or one phase of the recurring expansion/backlash cycle?',
    'Track bench composition, doctrine, and enforcement capacity over coming decades; compare against prior cycle amplitudes (Lochner-to-New Deal, Warren-to-Burger).',
    'Durable reversal dates a transition toward inertial maintenance of the remaining doctrine; cyclical continuation keeps the hybrid classification with oscillating metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dobbs_oscillation_or_reversal, empirical, 'Whether the endpoint decline is reversal or cycle phase.').

omega_variable(
    adaptation_usurpation_boundary,
    'Which interpretive moves count as legitimate adaptation of enacted principle, and which as usurpation of the amendment power reserved to the people?',
    'No purely empirical test exists; resolution comes from doctrinal theory — whether a ruling articulates principle traceable to text and history versus creating policy the text does not contain.',
    'Drawing the boundary higher shrinks the coordination component and raises extraction; drawing it lower does the reverse. The assessment of individual landmark rulings flips with the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_usurpation_boundary, conceptual, 'Boundary between principled adaptation and usurped amendment authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_constitution_reading_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t0, observed).
narrative_ontology:measurement(living_constitution_reading_tr_t20, us_constitution_interpretive__living_constitution_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t20, observed).
narrative_ontology:measurement(living_constitution_reading_tr_t40, us_constitution_interpretive__living_constitution_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t40, observed).
narrative_ontology:measurement(living_constitution_reading_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t60, observed).
narrative_ontology:measurement(living_constitution_reading_tr_t80, us_constitution_interpretive__living_constitution_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t80, observed).
narrative_ontology:measurement(living_constitution_reading_tr_t100, us_constitution_interpretive__living_constitution_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(living_constitution_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(living_constitution_reading_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(living_constitution_reading_be_t0, observed).
narrative_ontology:measurement(living_constitution_reading_be_t20, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(living_constitution_reading_be_t20, observed).
narrative_ontology:measurement(living_constitution_reading_be_t40, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(living_constitution_reading_be_t40, observed).
narrative_ontology:measurement(living_constitution_reading_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement_basis(living_constitution_reading_be_t60, observed).
narrative_ontology:measurement(living_constitution_reading_be_t80, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 80, 0.61).
narrative_ontology:measurement_basis(living_constitution_reading_be_t80, observed).
narrative_ontology:measurement(living_constitution_reading_be_t100, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(living_constitution_reading_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(living_constitution_reading_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(living_constitution_reading_su_t0, observed).
narrative_ontology:measurement(living_constitution_reading_su_t20, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(living_constitution_reading_su_t20, observed).
narrative_ontology:measurement(living_constitution_reading_su_t40, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(living_constitution_reading_su_t40, observed).
narrative_ontology:measurement(living_constitution_reading_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(living_constitution_reading_su_t60, observed).
narrative_ontology:measurement(living_constitution_reading_su_t80, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement_basis(living_constitution_reading_su_t80, observed).
narrative_ontology:measurement(living_constitution_reading_su_t100, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(living_constitution_reading_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the us_constitution_interpretive kernel: one colloquial label ('how the Constitution is interpreted') covers three structurally distinct arrangements. This file authors the living-constitution instantiation (evolving meaning, judicial adaptive authority); the originalist instantiation (fixed meaning) and the popular-constitutionalist instantiation (popular authorship) are separate stories with their own epsilon, beneficiary/victim sets, and classifications. Edges run between family members because each reading's dominance changes the others' operating environment: originalist capture of the appointing pipeline pressures this reading's doctrine, and this reading's courtroom centering pressures popular-constitutionalist channels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
