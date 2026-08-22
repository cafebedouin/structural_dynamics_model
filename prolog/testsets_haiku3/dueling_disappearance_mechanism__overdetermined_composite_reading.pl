% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Overdetermined Disappearance: Multiple Causal Pathways
 *   domain: legal/social/cultural
 *
 * SUMMARY:
 *   Dueling—the ritualized, formalized single combat between gentlemen to
 *   settle disputes of honor—declined sharply in the Atlantic world from the
 *   mid-18th century onward and was nearly extinct by the end of the 19th
 *   century. This constraint story instantiates the OVERDETERMINED-COMPOSITE
 *   reading: that dueling's disappearance was not caused by any single
 *   mechanism (legal prohibition, cultural shift, institutional substitution,
 *   or war trauma) acting alone, but by multiple independent sufficient
 *   conditions operating simultaneously and reinforcing one another. Each
 *   pathway would have been sufficient on its own to end dueling; together
 *   they made it structurally impossible. The reading rejects both the
 *   contraction reading (which credits cultural displacement of honor-culture
 *   by dignity-culture) and the institutional-displacement reading (which
 *   credits courts and formal law outcompeting dueling as
 *   dispute-resolution). Instead, it asserts that causal pathways are
 *   non-separable: legal prohibition, institutional modernization, cultural
 *   shift, and Civil War trauma all contributed, and no single epsilon can be
 *   measured because the constraint emerges from overdetermined interaction,
 *   not from a single extractive mechanism.
 *
 * KEY AGENTS:
 *   - legal_system_modernizers: institutional reformers pushing legal prohibition and court-based adjudication
 *   - honor_culture_practitioners: gentlemen bound by honor codes that make dueling a duty
 *   - institutional_dispute_resolution_bodies: courts and state authority expanding jurisdiction
 *   - dignity_culture_advocates: cultural reformers promoting shame-based ethics over honor-based ethics
 *   - civil_authority_consolidators: state officials establishing monopoly on legitimate violence
 *   - post_civil_war_trauma_bearers: gentry traumatized by mass violence and loss
 *   - anti_dueling_reformers: clergy, journalists, civic leaders mounting explicit campaigns
 *   - historical_analysts: scholars attempting to identify primary causal mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.68).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Overdetermined Disappearance: Multiple Causal Pathways").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "legal/social/cultural").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '5f8f0b18-46e2-4630-966c-36ab5e815a6b').
narrative_ontology:cs_kernel_codification('5f8f0b18-46e2-4630-966c-36ab5e815a6b', distributed).
narrative_ontology:cs_authority_grounding('5f8f0b18-46e2-4630-966c-36ab5e815a6b', distributed).
narrative_ontology:cs_reading_relation('5f8f0b18-46e2-4630-966c-36ab5e815a6b', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f8f0b18-46e2-4630-966c-36ab5e815a6b', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('5f8f0b18-46e2-4630-966c-36ab5e815a6b', foundational, causal_overdetermination_non_separable).
narrative_ontology:cs_axiom_status(causal_overdetermination_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('5f8f0b18-46e2-4630-966c-36ab5e815a6b', causal_overdetermination_non_separable, empirically_contingent).
narrative_ontology:cs_axiom('5f8f0b18-46e2-4630-966c-36ab5e815a6b', foundational, multiple_independent_sufficient_pathways).
narrative_ontology:cs_axiom_status(multiple_independent_sufficient_pathways, holdable).
narrative_ontology:cs_axiom_grounding('5f8f0b18-46e2-4630-966c-36ab5e815a6b', multiple_independent_sufficient_pathways, empirically_contingent).
narrative_ontology:cs_reference_frame('5f8f0b18-46e2-4630-966c-36ab5e815a6b', honor_culture_functional_dispute_resolution).
narrative_ontology:cs_drift_state('5f8f0b18-46e2-4630-966c-36ab5e815a6b', post_civil_war_institutional_modernity, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('5f8f0b18-46e2-4630-966c-36ab5e815a6b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_system_modernizers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_dispute_resolution_bodies).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_authority_consolidators).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_advocates).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, landed_gentry_honor_bound).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, middle_class_commercial_interests).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, religious_institutions).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_trauma_bearers).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, rational_dispute_resolution_over_honor_codes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement and enforce legal prohibitions on dueling; establish courts and statutory law as the authorized dispute-resolution mechanism. Justify the move as replacing honor-based violence with rational adjudication. Benefit from the consolidation of authority and the expansion of formal legal jurisdiction into domains previously governed by custom.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_system_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).

% Bound by honor codes that make dueling a non-negotiable response to insult; dueling is the mechanism through which male honor and social standing are defended and vindicated. Legal prohibition creates a bind: honor demands the duel, but law forbids it and imposes criminal penalty. Identity as a gentleman is fused with the capacity and willingness to fight.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, national).

% Courts, legislative bodies, and administrative bodies expand their jurisdiction and authority as dueling declines. Absorb the dispute-resolution function dueling once served and gain legitimacy and power from offering an alternative mechanism. The substitution strengthens these institutions relative to custom-based adjudication.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_dispute_resolution_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_dispute_resolution_bodies, agenda_setter).

% Dueling is a mechanism for defending honor and property disputes in a world where land and name define status. Legal systems and courts displace dueling as the legitimate way to settle conflicts. The loss of dueling as an available response threatens the social standing and honor of gentry whose identity is built around the capacity to defend their interests through combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, landed_gentry_honor_bound, payer,
    powerful, biographical, identity_locked, regional).

% Promote a cultural shift from honor-based ethics (where public insult demands violent response) to dignity-based ethics (where individual worth is internal and does not require violent vindication). Dueling is positioned as barbaric and irrational; its disappearance is framed as moral and cultural progress. Benefit from the reframing of honor-culture as primitive.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, dignity_culture_advocates, beneficiary,
    moderate, generational, mobile, national).

% Central government and state authority expand by claiming the monopoly on legitimate violence. Dueling represents private violence outside state control. The elimination of dueling strengthens the state's claim to exclusive authority over force and dispute resolution. Military institutions and state police gain legitimacy as the only authorized violence apparatus.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_authority_consolidators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_authority_consolidators, agenda_setter).

% In the American South and border states, the Civil War creates massive trauma and loss of life among the gentry and planter classes. The scale of the war makes the ritualized, controlled violence of dueling appear anachronistic and frivolous by comparison. Veterans and those who have survived mass violence are less inclined to treat honor-satisfaction through individual combat as legitimate or necessary.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_trauma_bearers, payer,
    powerful, biographical, constrained, regional).

% Dueling has always been primarily a gentry practice; middle-class merchants and professionals do not rely on honor-based dispute resolution. Their economic interests are served by courts and contracts. The decline of dueling eliminates a status symbol exclusive to the gentry and accelerates the cultural shift from honor-based to dignity-based and commercial-based status markers.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, middle_class_commercial_interests, beneficiary,
    organized, generational, mobile, national).

% Social reformers, clergy, journalists, and civic leaders mount an explicit campaign against dueling through writing, preaching, and advocacy. They frame dueling as murder, barbarism, and a violation of both religious and rational principles. Their advocacy shifts the cultural narrative and strengthens legal enforcement against dueling.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, anti_dueling_reformers, agenda_setter,
    organized, generational, mobile, national).

% Churches and religious doctrine have opposed dueling on theological grounds (prohibition of murder, violation of divine will). As dueling becomes culturally stigmatized, religious authority over moral questions is reinforced. Religious institutions gain legitimacy in defining what constitutes honorable behavior, displacing the secular honor code.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, religious_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Systematically excluded from both dueling (the practice) and the honor codes that justified it, yet often the nominal occasion for duels (insults to female relatives triggered male honor responses). Their exclusion from the conversation about honor and violence means their actual interests and safety concerns are not represented in the discourse about dueling's decline or its replacement mechanisms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, women_as_excluded_agents, excluded,
    powerless, biographical, trapped, national).

% Scholars and historians examine the causes of dueling's decline and attempt to identify which mechanism was primary or causally sufficient. The overdetermined-composite reading asserts that no single cause is sufficient; multiple independent pathways converged to make dueling socially and legally impossible, and the causal pathways are non-separable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_dispute_resolution_bodies).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling historically resolved disputes over honor and defended social standing in a world where courts and legal systems did not exist or were not legitimate arbiters of personal honor. It coordinated expectations about how insults are answered and how male status is maintained in honor-culture societies.
% TRANSFER_FUNCTION: Dueling transferred risk of death and injury to honor-bound individuals who perceived a duty to fight rather than accept insult. It also transferred social status validation from institutional sources (law, courts, commerce) to honor-culture sources (the duel, witnesses, acceptance of challenge). The mechanism moved potential disputes from formal legal channels into private violent resolution.
% ABSENT_VOICES: Women were systematically excluded from both the practice of dueling and the discourse about honor codes, despite being the nominal occasion for many duels. Lower classes and non-gentry individuals could not participate in dueling and had no voice in the honor-culture rules; their alternative dispute mechanisms (brawling, public humiliation, ostracism) were neither recognized nor debated. Former slaves and newly enfranchised groups had no seat at the table when honor-based social codes were being displaced by legal frameworks.
% DISAPPEARANCE_RATIONALE: If dueling and its honor-culture framework had persisted, the social standing of gentry would continue to rest on willingness to fight, property disputes would be settled through combat rather than courts, and the authority of the state to define legitimate violence would remain contested. The legal system, state authority, dignity-culture values, and middle-class commercial status markers would all have developed differently. The world that emerged—one where courts handle disputes, where honor is internalized rather than externally vindicated, where the state has monopoly on legitimate violence—would not exist in the same form.
% FOUNDING_PROBLEM: In pre-modern and early-modern honor-culture societies, disputes over insults, property, and status could not be adjudicated through formal legal systems (which either did not exist or were not trusted by the gentry). Dueling provided a mechanism for individuals to defend their honor and resolve status disputes in a world where honor was the primary currency of social standing for the landowning classes.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and social historians outside the honor-culture tradition (scholars working from dignity-culture or institutional-modernization frameworks) attest that formal legal systems have become legitimate and effective dispute-resolution mechanisms. Court records show dueling cases being prosecuted successfully. Demographic data from post-Civil War America shows sharp declines in dueling deaths in jurisdictions that combined legal prohibition with institutional development and cultural shift. Historians studying the gentry and landed classes document the shift from honor-based to dignity-based and commercial-based status markers.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction metric (0.68) reflects the constraint's operation as experienced by honor-bound practitioners: legal prohibition extracts the right to respond to insult; institutional substitution extracts the authority of honor-culture dispute resolution; cultural shift extracts the legitimacy of honor-based status; war trauma extracts the willingness and capacity to fight. Suppression (0.71) is high because the constraint's persistence depends on active enforcement of legal prohibition, institutional exclusion of dueling as legitimate dispute resolution, cultural stigmatization of honor-codes, and the social trauma that makes dueling unthinkable. Theater (0.42) is moderate: by the end of the interval, dueling is largely gone, but residual theatrical performances of honor persist in formalized insult and apology rituals that preserve the form while the substance (willingness to fight) has been drained away. The time series tracks the accumulation of suppression-requirement as multiple causal pathways converge: at t=0, legal prohibition alone is insufficient; by t=60, the overdetermined weight makes dueling structurally impossible. Accessibility_collapse (0.72) is high because once all four causal pathways are understood, the alternative (accepting insult without fighting) becomes the only available choice; honor-bound individuals are trapped between honor and law, a bind with no exit. Resistance (0.58) is moderate: honor-culture practitioners resist throughout the interval, but their resistance is fragmented (different mechanisms are targeted by different reformers) and ultimately overwhelmed by the combined weight of legal, institutional, cultural, and traumatic pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the modernizer/beneficiary seats, dueling's disappearance is progress: the replacement of irrational violence with rational adjudication, private disorder with state order, barbarism with civilization. From the honor-bound payer seats, the disappearance is destruction: the removal of the only mechanism through which honor can be defended, the imposition of a framework (dignity, dignity, law) that contradicts the core of their self-understanding. From the analytical observer seat, the reading is overdetermined interaction: each causal pathway (legal, institutional, cultural, traumatic) is sufficient on its own; together they are overwhelming. The overdetermined reading prevents collapsing this perspectival gap into a single causal story that would favor one seat's narrative over another.
 *
 * DIRECTIONALITY LOGIC:
 *   The four beneficiary groups (legal modernizers, institutional bodies, dignity-culture advocates, civil authority consolidators) all gain from dueling's decline but through different mechanisms: legal system modernizers gain authority and jurisdiction; institutional bodies gain power and legitimacy; dignity-culture advocates gain cultural dominance; civil authority consolidators gain state power. The two payer groups (honor-culture practitioners and landed gentry) both lose, but through the same mechanism: dueling is the practice through which their honor is constituted and defended. The identity-lock for honor-bound practitioners is structural: in a honor-culture framework, accepting insult without fighting means accepting dishonor, which unmans the individual and destroys social standing. No alternative mechanism (courts, apology, legal redress) can serve the same function in the honor-culture frame. Exit from the practice means exit from the culture, which is exit from identity itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dispute resolution in honor-culture societies without legitimate legal systems) is dead: formal legal systems are now legitimate and effective. The world_rearranges verdict confirms that dueling's disappearance causes reorganization: state authority, institutional legitimacy, cultural values, and social status markers all shift when dueling is gone. The mandatrophy signal (dead founding problem + world_rearranges) indicates that the constraint should no longer exist—it has become theatrical maintenance without functional purpose. However, the constraint persists due to the overdetermined nature of its implementation: legal prohibition remains on the books even though dueling has ceased; institutional frameworks remain even though they have achieved their purpose; cultural stigma persists even though it is no longer needed; and trauma narratives are transmitted even though the war is over. The theater_ratio's rise (from 0.15 to 0.42) tracks this degradation from functional to performative: by the end of the interval, dueling is already nearly extinct, but the legal, institutional, and cultural machinery persists as theater. The constraint is transitioning from tangled_rope (coordination + extraction) to piton (inertial maintenance of mechanisms whose function has been achieved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_separability,
    'Are the four causal pathways (legal prohibition, institutional substitution, cultural shift, civil war trauma) truly independent and separately sufficient, or are some of them causally downstream of others (e.g., does legal prohibition cause cultural shift, or does cultural shift enable legal prohibition)?',
    'Detailed historical analysis of the temporal sequence and causal claims in legal, institutional, cultural, and military records from the relevant period; identification of jurisdictions or time periods where one pathway operated without others to test sufficiency; counterfactual analysis of what would have happened if one pathway had been absent.',
    'If pathways are truly independent and separately sufficient, the constraint is genuinely overdetermined and no single ε can be measured. If one pathway is downstream of another, the causal structure simplifies and one mechanism becomes primary, shifting the constraint type toward a single-cause reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_separability, empirical, 'Whether the four causal pathways for dueling''s decline are truly independent or causally entangled.').

omega_variable(
    natural_decline_vs_enforced_suppression,
    'Would dueling have declined to extinction through cultural and institutional change even without legal prohibition and state enforcement, or is legal suppression structurally necessary to complete the transition?',
    'Comparative historical analysis of jurisdictions with strong anti-dueling laws vs. jurisdictions with minimal legal prohibition; tracking of dueling prevalence in legal vs. non-legal enforcement contexts; examination of whether cultural shift preceded or followed legal prohibition in various societies.',
    'If dueling would naturally decline without enforcement, the constraint is primarily cultural-institutional coordination with cultural beneficiaries (dignity advocates) and cultural victims (honor practitioners); suppression becomes secondary. If legal enforcement is structurally necessary, the constraint remains a tangled_rope with state authority consolidators as primary beneficiaries and honor-bound practitioners as primary targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_decline_vs_enforced_suppression, empirical, 'Whether dueling''s decline was natural cultural shift or enforced legal suppression.').

omega_variable(
    honor_culture_vitality_ambiguity,
    'At the start of the interval (t=0), is honor-culture still genuinely vital and self-sustaining, or is it already declining due to invisible structural changes (economic, demographic, institutional) that predate the measured causal pathways?',
    'Demographic analysis of who participates in dueling and whether participation is already contracting before legal prohibition; economic analysis of whether honor-culture is still functional for the gentry''s primary economic interests; institutional analysis of whether alternative dispute-resolution mechanisms are already substituting before formal legal prohibition.',
    'If honor-culture is still vital at t=0, the causal pathways are acting against genuine alternative. If honor-culture is already moribund, the constraint may be better characterized as piton (theatrical maintenance of an already-dead practice) rather than tangled_rope (active coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_vitality_ambiguity, empirical, 'Whether honor-culture remains vital or is already declining at the start of the measured interval.').

omega_variable(
    civil_war_trauma_as_independent_pathway,
    'Is the Civil War''s traumatic impact on honor-culture practitioners an independent causal pathway, or is it an amplifier of cultural and institutional shifts that are already underway?',
    'Comparative analysis of dueling decline rates in the antebellum vs. postbellum American South and border states; analysis of dueling in non-combatant regions (Britain, parts of continental Europe) to test whether trauma is necessary or merely accelerating; testimony and records from combat-zone gentry about their changed attitudes toward honor-based violence.',
    'If trauma is independent, it confirms overdetermination: cultural, institutional, legal, AND traumatic factors all contribute. If trauma is an amplifier, the three primary mechanisms (cultural, institutional, legal) are the drivers, and trauma accelerates the process but is not separately sufficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_war_trauma_as_independent_pathway, empirical, 'Whether Civil War trauma is an independent causal pathway or an amplifier of other mechanisms.').

omega_variable(
    reading_boundary_overdetermined_vs_institutional,
    'At what point does the overdetermined-composite reading cease to distinguish itself from the institutional-displacement reading? If institutional substitution (courts replacing dueling) is one of four equally weighted pathways, what makes this reading ''overdetermined'' rather than just ''institutional with additional factors''?',
    'Conceptual analysis of what constitutes ''equal weighting'' of pathways and how overdetermination differs structurally from institutional substitution with supporting factors; historiographical review of how scholars have framed dueling''s decline (do they use language of ''multiple independent causes'' or ''institutional displacement as primary''?); explicit definition of the threshold at which the reading''s claim shifts from overdetermined to institutional-primary.',
    'If the reading cannot clearly distinguish itself from the institutional reading, the kernel contest may be overconstrained and one reading should be merged or eliminated. If the reading can clearly articulate the distinction, it remains a live alternative with different ε implications and victim/beneficiary structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_overdetermined_vs_institutional, conceptual, 'Boundary between the overdetermined-composite reading and the institutional-displacement reading.').

omega_variable(
    kernel_stability_across_readings,
    'Is the dueling-disappearance kernel stable across all three readings, or do the readings diverge so far in their framing of what happened that they are describing different kernels altogether?',
    'Explicit comparative analysis of the three readings'' assumptions about causation, structure, and outcome; identification of facts or events both readings must explain; testing whether a fact that one reading treats as primary the other treats as secondary or denies entirely.',
    'If the readings diverge on fundamental facts or on what counts as evidence, the kernel boundary is unstable and the three readings may be reading different kernels that happen to be labeled with the same historical event. If they can agree on core facts but disagree about causation and weight, they remain readings of a single kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_stability_across_readings, conceptual, 'Whether the dueling-disappearance kernel is stable across all three claimed readings or whether readings are describing different kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(duel_tr_t0, observed).
narrative_ontology:measurement(duel_tr_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(duel_tr_t10, observed).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(duel_tr_t20, observed).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(duel_tr_t30, observed).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(duel_tr_t40, observed).
narrative_ontology:measurement(duel_tr_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(duel_tr_t50, observed).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(duel_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(duel_be_t0, observed).
narrative_ontology:measurement(duel_be_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(duel_be_t10, observed).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(duel_be_t20, observed).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(duel_be_t30, observed).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(duel_be_t40, observed).
narrative_ontology:measurement(duel_be_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(duel_be_t50, observed).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(duel_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(duel_su_t0, observed).
narrative_ontology:measurement(duel_su_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(duel_su_t10, observed).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(duel_su_t20, observed).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(duel_su_t30, observed).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(duel_su_t40, observed).
narrative_ontology:measurement(duel_su_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(duel_su_t50, observed).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(duel_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% The dueling-disappearance kernel has three readings: contraction (cultural displacement of honor by dignity), institutional-displacement (courts replacing dueling), and overdetermined-composite (this one, asserting non-separable multi-pathway causation). Each reading is a distinct constraint story with different epsilon, beneficiary structure, and type. The contraction reading models dueling's decline as primarily cultural value-shift; the institutional-displacement reading models it as institutional competition; the overdetermined-composite reading asserts causal pathways are inseparable and no single ε can be measured because four sufficient conditions act simultaneously. The three stories are linked via network.affects_constraints to enable comparative analysis of how different causal framings produce different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
