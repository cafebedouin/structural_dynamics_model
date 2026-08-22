% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor-Satisfaction Code as Persisting Normative Substrate (Practice-Decline Reading)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This story instantiates the practice-decline reading of the
 *   honor-satisfaction kernel: dueling as a literal practice collapsed across
 *   the 18th-to-20th-century interval not because elite men stopped believing
 *   insults required visible vindication, but because states criminalized
 *   dueling, courts and militaries built alternative disciplinary tracks, and
 *   the opportunity cost of risking death or imprisonment for a point of
 *   honor rose past what most disputants would pay. Under this reading the
 *   underlying normative substrate — that reputational injury demands a
 *   public, costly, honor-restoring response — survived largely intact and
 *   now expresses itself in attenuated forms: military codes of honor,
 *   courts-martial for insult-adjacent conduct, defamation litigation, and
 *   the ethnographically documented 'culture of honor' patterns in the
 *   American South and comparable herding/frontier societies elsewhere. This
 *   is a rope, not a mountain: the code coordinates a real status-vindication
 *   problem among honor-bound elites and mostly-military institutions, and
 *   the mechanism can (and largely did) survive the specific enforcement
 *   pressure that killed only its most legally exposed enactment (dueling),
 *   while the coordination function persists elsewhere. Two sibling
 *   constraints read the same historical kernel differently and are NOT this
 *   constraint: cultural_contraction_reading holds that the honor code itself
 *   underwent foundational transformation into a 'culture of dignity,' making
 *   dueling literally unthinkable rather than merely impractical (a
 *   mountain-erosion-style account of value change);
 *   composite_overdetermined_reading holds that exogenous suppression and
 *   endogenous delegitimation operated jointly and non-independently,
 *   resisting decomposition into either single mechanism. Each sibling
 *   instantiates its own ε and its own stakeholder structure; this file
 *   speaks only for the practice-decline account.
 *
 * KEY AGENTS:
 *   - honor_bound_elite_men: primary beneficiary of the persisting substrate (powerful/identity_locked) — status economy still runs on honor, only settlement method changed
 *   - military_officer_corps: institutional administrator and beneficiary (institutional/constrained) — formalizes the substrate into codes of conduct and courts of honor
 *   - dueling_participants_and_families: historical payers (powerless/trapped) — bore literal mortal risk under the pre-suppression regime
 *   - women_excluded_from_the_honor_economy: structurally excluded payer (powerless/trapped) — cited justification for disputes, denied standing to resolve them
 *   - the_state_legal_system: agenda_setter supplying the exogenous enforcement this reading identifies as causally decisive
 *   - historians_of_honor_and_violence: analytical observers whose dating of decline against prosecution intensity is this reading's key evidentiary basis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.42).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.38).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor-Satisfaction Code as Persisting Normative Substrate (Practice-Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'ff0f05e1-bdd1-4463-af2d-e9904635632b').
narrative_ontology:cs_kernel_codification('ff0f05e1-bdd1-4463-af2d-e9904635632b', distributed).
narrative_ontology:cs_authority_grounding('ff0f05e1-bdd1-4463-af2d-e9904635632b', practice).
narrative_ontology:cs_interpretation_layer_present('ff0f05e1-bdd1-4463-af2d-e9904635632b').
narrative_ontology:cs_reading_relation('ff0f05e1-bdd1-4463-af2d-e9904635632b', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff0f05e1-bdd1-4463-af2d-e9904635632b', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('ff0f05e1-bdd1-4463-af2d-e9904635632b', foundational, honor_norm_survived_intact_under_suppression).
narrative_ontology:cs_axiom_status(honor_norm_survived_intact_under_suppression, holdable).
narrative_ontology:cs_axiom_grounding('ff0f05e1-bdd1-4463-af2d-e9904635632b', honor_norm_survived_intact_under_suppression, empirically_contingent).
narrative_ontology:cs_axiom('ff0f05e1-bdd1-4463-af2d-e9904635632b', secondary, exogenous_enforcement_is_sufficient_cause_of_practice_decline).
narrative_ontology:cs_axiom_status(exogenous_enforcement_is_sufficient_cause_of_practice_decline, holdable).
narrative_ontology:cs_axiom_grounding('ff0f05e1-bdd1-4463-af2d-e9904635632b', exogenous_enforcement_is_sufficient_cause_of_practice_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('ff0f05e1-bdd1-4463-af2d-e9904635632b', eighteenth_century_elite_honor_code).
narrative_ontology:cs_drift_state('ff0f05e1-bdd1-4463-af2d-e9904635632b', mid_twentieth_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff0f05e1-bdd1-4463-af2d-e9904635632b', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_bound_elite_men).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, southern_regional_status_hierarchy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, dueling_participants_and_families).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, women_excluded_from_the_honor_economy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, non_elite_men_denied_standing_to_claim_satisfaction).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, reputational_stakes_require_visible_vindication).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__practice_decline_reading, personal_courage_is_publicly_demonstrable_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentry, officers, and professionals whose social standing was historically validated through willingness to issue or accept a challenge. As dueling becomes legally and practically unavailable, they retain the underlying code — insult still demands response — but substitute litigation, public denunciation, institutional complaint, or ritualized non-lethal confrontation. Their status economy still runs on honor; only the settlement mechanism has been foreclosed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_elite_men, beneficiary,
    powerful, generational, identity_locked, national).

% Maintains formal codes of conduct, courts of honor, and disciplinary boards that preserve the logic of honor-satisfaction (insult, demand, adjudication, restored standing) inside a chain of command that has banned actual combat between officers. The institution administers the substrate directly and could relax it, but the code's persistence legitimizes hierarchy and esprit de corps.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, agenda_setter).

% A regional culture (the 'culture of honor' literature) in which reputational sensitivity, willingness to retaliate against insult, and defense of family name remain socially rewarded long after formal dueling vanished. Local status is still partly allocated by demonstrated honor-defense, now expressed through interpersonal violence, litigation, or social ostracism rather than sanctioned combat.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, southern_regional_status_hierarchy, beneficiary,
    organized, civilizational, identity_locked, regional).

% Individuals historically compelled by the code to risk death or injury to avoid social ruin, and their families who bore the loss when duels went forward or the shame when they refused. Even as the practice declines, men still perceive some insults as demanding physical or reputational risk they cannot decline without status collapse — the exit from any single duel does not exit the underlying obligation structure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, dueling_participants_and_families, payer,
    powerless, biographical, trapped, local).

% Structurally barred from issuing or accepting challenges, yet frequently the ostensible cause of them (defense of a wife's or sister's virtue) and the ones left widowed or shamed by outcomes. The substrate's persistence in attenuated form (chivalric honor rhetoric, family-name defense) continues to position them as property to be defended rather than parties with standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, women_excluded_from_the_honor_economy, payer,
    powerless, generational, trapped, national).

% Working-class and non-gentry men were historically excluded from the code's protections — an insult from a gentleman could not be answered by a duel challenge from someone deemed beneath honor-bearing status, leaving them without the code's remedy while still subject to its status logic (deference, insult-avoidance) from below.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, non_elite_men_denied_standing_to_claim_satisfaction, payer,
    powerless, biographical, trapped, national).

% Criminalized dueling, prosecuted participants and seconds, and offered civil defamation and assault remedies as substitutes. The state supplied the exogenous enforcement this reading identifies as the actual mechanism of practice decline — not persuasion that honor itself was illegitimate, but the raising of the practical cost of settling honor disputes by combat above what most disputants would pay.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, the_state_legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Study court records, dueling statistics, and honor-code texts across the 19th and 20th centuries. Under this reading, they attribute the decline curve's timing to prosecution intensity and professional/opportunity cost rather than to a documented shift in what people believed honor required — the belief structure is read as intact, its enactment as suppressed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, historians_of_honor_and_violence, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor-satisfaction code solves a real coordination problem in status hierarchies without centralized courts of reputation: it specifies a mutually recognized procedure (challenge, seconds, terms, field) by which a reputational injury can be publicly and finally resolved, restoring both parties' standing regardless of outcome, without requiring an external adjudicator both sides trust.
% TRANSFER_FUNCTION: The code transfers physical risk and, historically, life itself from the initiator of the coordination problem (the insult) onto the bodies of the disputants, while transferring restored status to whichever party's conduct satisfies the code's honor criteria (accepting the challenge, standing ground) regardless of the dispute's underlying merits. It also transfers standing itself — asymmetrically granting the right to demand satisfaction to gentry men and withholding it from women and non-elites.
% ABSENT_VOICES: Women, whose honor was frequently the stated stake but who had no standing to issue or answer challenges, are structurally excluded from the procedure while remaining its most common cited justification. Non-elite men denied dueling standing are also absent from the code's protections despite being subject to its status logic from below.
% DISAPPEARANCE_RATIONALE: The reading's own claim is that the code's disappearance is exactly what has NOT happened — dueling as a practice declined, but the code persists as normative substrate in military honor codes, defamation law's continued cultural resonance, and regional culture-of-honor patterns. Whether 'the world rearranges' depends on which layer is asked: removing the residual code from military discipline or regional social norms would visibly rearrange those institutions; removing the (already-vanished) dueling practice itself would change nothing further, since it is already gone. The parties dispute which layer is the real referent, which is why this question is itself contested under this reading.
% FOUNDING_PROBLEM: Pre-modern and early-modern elite societies lacked a trusted, sufficiently fast, and status-preserving mechanism for resolving accusations of dishonor between social equals absent police forces or courts that gentlemen recognized as competent to judge matters of personal reputation.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and legal historians attest the practical problem (violent self-help dispute resolution among elites) was substantially solved by state monopolization of legitimate force and expanded civil/criminal remedies — an outside-the-honor-economy verdict. Military institutions and regional culture-of-honor communities, who are among the beneficiaries this reading names, attest the underlying problem (reputational injury requiring visible vindication) remains live, which is a self-interested corroboration this reading treats with appropriate skepticism rather than adopting outright.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 rather than high, because under this reading the code's persistence is substantially coordination-functional (resolving reputational disputes without full-scale interpersonal violence in its residual, non-lethal forms) even though it retains asymmetric costs for the historically excluded groups. Suppression is authored rising sharply over the interval (0.2 to 0.75 on the suppression_requirement series) because the STORY's central claim is that legal and institutional suppression of the literal dueling PRACTICE, not delegitimation of the underlying norm, is what drove the decline — the state had to work increasingly hard (statute, prosecution, military discipline, professional consequence) to suppress a norm-consistent practice that people still, by this reading's lights, considered legitimate. Theater ratio rises in parallel (0.15 to 0.55) because as literal dueling becomes impractical, an increasing share of 'honor defense' activity becomes performative — ritualized apology demands, symbolic courts of honor, culture-of-honor posturing — substituting for the substantive risk-bearing the original code required, exactly as the expected structural delta predicts ('dueling remains thinkable but impractical').
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (the state legal system), the arrangement looks like successful suppression of a dangerous private-violence practice — a public-safety win. From the beneficiary seats (elite men, officer corps, regional honor culture), the same history looks like an intact normative commitment that simply lost one of its enactment channels — the code did not fail, only one venue for satisfying it did. From the payer seats (excluded women, non-elite men, and historically the dueling participants themselves), the substrate's persistence in attenuated form means the asymmetric costs (reputational vulnerability, exclusion from remedy, being cited as stakes without having standing) continue regardless of whether the specific practice of dueling survives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (honor-bound elite men, the officer corps, the regional status hierarchy) are declared with low-to-moderate directionality: the substrate subsidizes their status economy and they retain meaningful institutional or cultural exit options even where identity-locked. Victims (dueling participants historically, excluded women, non-elite men) are declared with high directionality: they bear costs — mortal risk, exclusion from remedy, subordinate status — without corresponding institutional power to renegotiate the code's terms. The state legal system is deliberately NOT declared as beneficiary or victim; it is the agenda_setter whose enforcement activity is this reading's causal mechanism, analytically positioned rather than benefiting from or paying into the honor economy itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists two mislabeling errors symmetrically. It does not mislabel the decline of dueling as evidence the underlying coordination problem (reputational injury requiring public vindication) disappeared — the persistence of military codes, defamation law, and culture-of-honor patterns is offered as direct evidence the mandate is still live in attenuated form, which is why mandatrophy_resolved is NOT declared here. It also does not mislabel the code as pure extraction requiring no genuine coordination function — the rope classification is claimed precisely because a real coordination problem (status-preserving dispute resolution without a trusted external adjudicator) persists and is still served, just via lower-cost substitute mechanisms once the state raised the cost of the original enactment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practice_decline_vs_norm_transformation,
    'Did dueling decline because the state made it too costly to enact a still-endorsed norm (this reading), or because the underlying honor code itself was delegitimized so that dueling became normatively unthinkable rather than merely practically foreclosed (the cultural_contraction_reading)?',
    'Comparative analysis of jurisdictions and time periods where legal suppression was weak or absent but dueling still declined (or conversely, where suppression was strong but dueling persisted underground) would separate the enforcement-driven mechanism from a norm-shift mechanism; survey and diary evidence of whether elite men in the late decline period still regarded refusing a challenge as dishonorable (supporting this reading) versus regarded the entire premise as archaic (supporting the sibling) is the direct test.',
    'If evidence shows the norm itself eroded independent of enforcement, this reading''s core premise fails and the constraint should be reclassified toward the cultural_contraction_reading''s mountain-erosion-style account rather than a rope account of coordination-under-suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_decline_vs_norm_transformation, empirical, 'Whether the decline mechanism is enforcement-driven persistence of an intact norm, or norm transformation itself.').

omega_variable(
    attenuated_forms_as_genuine_substrate_survival,
    'Are military honor codes, courts of honor, and ''culture of honor'' regional patterns genuine continuations of the same normative substrate that generated dueling, or are they structurally distinct institutions that happen to share vocabulary with the historical honor code?',
    'Trace institutional lineage and self-description: do military codes of conduct explicitly invoke the same honor-satisfaction logic (challenge, adjudication, restored standing) as historical dueling codes, or do they derive from independent disciplinary logics that merely borrow the language of honor?',
    'If the attenuated forms are institutionally discontinuous rather than genuine survivals, the claim that the substrate ''persists'' is weakened and the constraint moves closer to describing a historical artifact with modern institutions coincidentally sharing terminology, undermining the rope classification''s coordination-function claim for the present day.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attenuated_forms_as_genuine_substrate_survival, conceptual, 'Whether attenuated honor institutions are true substrate survivals or independently-derived look-alikes.').

omega_variable(
    decomposability_of_exogenous_and_endogenous_causes,
    'Can exogenous legal/institutional suppression and endogenous normative delegitimation be cleanly separated as causal factors, or are they necessarily entangled (as the composite_overdetermined_reading claims)?',
    'Historical process-tracing of specific jurisdictions'' legislative debates: did legislators and elites justify anti-dueling statutes on the grounds that honor itself was illegitimate (suggesting entanglement) or purely on public-order/state-monopoly-of-violence grounds independent of any view on honor''s legitimacy (supporting clean separability)?',
    'If the causal pathways prove non-separable in the historical record, this reading''s clean exogenous-suppression account becomes an analytical simplification rather than an accurate causal history, and the composite reading becomes the better-supported account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decomposability_of_exogenous_and_endogenous_causes, conceptual, 'Whether this reading''s separation of exogenous and endogenous causes survives close historical scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(hono_tr_t1790, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1790, 0.2).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1830, 0.3).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1870, 0.4).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1910, 0.48).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1950, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1790, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1790, 0.52).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1830, 0.5).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1870, 0.47).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1910, 0.44).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement(hono_su_t1790, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1790, 0.3).
narrative_ontology:measurement(hono_su_t1830, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1830, 0.45).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement(hono_su_t1910, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_satisfaction_substrate kernel, each authored as a separate ε-invariant story per the decomposition principle. practice_decline_reading (this file) authors ε=0.42 and claims rope, holding the honor code persisted intact under exogenous suppression. cultural_contraction_reading authors a distinct ε reflecting genuine norm transformation (dignity culture displacing honor culture), which should classify closer to a natural erosion pattern rather than enforced-coordination-under-pressure. composite_overdetermined_reading authors a third ε reflecting non-separable joint causation. All three are linked bidirectionally via affects_constraints; none is the 'correct' reading — they are structurally distinct constraints sharing a historical kernel, and the corpus should treat divergence between their classifications as the intended signal of contested causal interpretation, not as an error to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
