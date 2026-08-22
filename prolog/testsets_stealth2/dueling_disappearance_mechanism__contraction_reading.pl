% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Substrate Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   Between roughly 1760 and 1914, the European honor economy — in which a
 *   gentleman's worth was conferred by peer recognition and defended by the
 *   duel — was displaced by a dignity culture in which worth is intrinsic and
 *   insult obligates forbearance rather than combat. This story instantiates
 *   the contraction_reading of the dueling_disappearance_mechanism kernel:
 *   the claim that dueling became culturally UNTHINKABLE — not merely
 *   forbidden or outcompeted — because dignity-culture axioms displaced
 *   honor-culture axioms at the level of what responses are available to
 *   thought at all. The epsilon referent is the standing dignity-culture
 *   arrangement as this reading sees it: a largely genuine civilizational
 *   advance that nonetheless bears a real extraction tail — the honor-culture
 *   practitioners whose entire meaning-framework became illegible,
 *   criminalized, or ridiculous, and the transitional generation caught
 *   between both regimes' demands. KEY AGENTS (by structural relationship): -
 *   honor_culture_practitioners: primary target (powerful/identity_locked) —
 *   bears the illegibility of their framework -
 *   transitional_generation_elites: secondary target (powerful/trapped) —
 *   double-bound between regimes - challenge_vulnerable_men: primary
 *   beneficiary (moderate/constrained) — freed from coerced lethal risk -
 *   bourgeois_professional_classes: principal collector (organized/mobile) —
 *   gains normative hegemony - dignity_culture_moralists: propagating
 *   beneficiary (organized/mobile) — collects cultural authority -
 *   women_of_honor_society: excluded voice (powerless/trapped) — disposed of
 *   by both regimes - historical_sociologists: analytical observer
 *   (analytical/analytical) — sees the full structure. Sibling readings
 *   (institutional displacement, overdetermined composite) are separate
 *   constraints in separate files; this story does not average over them.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: primary target (powerful/identity_locked) — aristocratic and officer-class men whose obligatory responses became unintelligible; exit requires self-repudiation
 *   - transitional_generation_elites: secondary target (powerful/trapped) — the mid-transition cohort bound by honor's commands and dignity's punishments simultaneously
 *   - challenge_vulnerable_men: primary beneficiary (moderate/constrained) — men unable to survive the challenge ritual, freed from a mortal hazard they could not previously decline
 *   - bourgeois_professional_classes: principal collector (organized/mobile) — their self-worth model ratified as the definition of civilized personhood
 *   - dignity_culture_moralists: propagating beneficiary (organized/mobile) — clergy, reformers, novelists collecting cultural authority as their axioms prevail
 *   - women_of_honor_society: excluded voice (powerless/trapped) — reputational collateral in both regimes, consulted by neither
 *   - historical_sociologists: analytical observer (analytical/analytical) — reconstructs the transition from archives and statistics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.32).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.18).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Substrate Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '8cc3297b-d9c0-4378-819f-12be09314cde').
narrative_ontology:cs_kernel_codification('8cc3297b-d9c0-4378-819f-12be09314cde', distributed).
narrative_ontology:cs_authority_grounding('8cc3297b-d9c0-4378-819f-12be09314cde', expertise).
narrative_ontology:cs_interpretation_layer_present('8cc3297b-d9c0-4378-819f-12be09314cde').
narrative_ontology:cs_reading_relation('8cc3297b-d9c0-4378-819f-12be09314cde', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cc3297b-d9c0-4378-819f-12be09314cde', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('8cc3297b-d9c0-4378-819f-12be09314cde', foundational, unthinkability_not_prohibition).
narrative_ontology:cs_axiom_status(unthinkability_not_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('8cc3297b-d9c0-4378-819f-12be09314cde', unthinkability_not_prohibition, empirically_contingent).
narrative_ontology:cs_axiom('8cc3297b-d9c0-4378-819f-12be09314cde', foundational, axiomatic_substrate_irreversibility).
narrative_ontology:cs_axiom_status(axiomatic_substrate_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('8cc3297b-d9c0-4378-819f-12be09314cde', axiomatic_substrate_irreversibility, empirically_contingent).
narrative_ontology:cs_reference_frame('8cc3297b-d9c0-4378-819f-12be09314cde', dignity_axiom_substrate_transition).
narrative_ontology:cs_drift_state('8cc3297b-d9c0-4378-819f-12be09314cde', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8cc3297b-d9c0-4378-819f-12be09314cde', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, challenge_vulnerable_men).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_classes).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_moralists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, transitional_generation_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European aristocratic and officer-class men socialized into the honor economy, in which worth is conferred by peer recognition and insult must be answered by challenge. As dignity axioms spread, their obligatory responses became criminal, ridiculous, or simply unintelligible: refusing the duel destroyed them inside their own framework, performing it destroyed them in the new one. Leaving would require repudiating the self their entire formation built — their lineage, schooling, regiment, and marriage market all ran on honor's grammar.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, generational, identity_locked, continental).

% The cohort maturing mid-transition (roughly 1780-1840), for whom honor still commanded the duel while law and polite opinion already punished it. They face a double bind with no clean script: comply with honor and face prosecution and infamy; comply with dignity and suffer social death in the army regiments, court circles, and clubs still operating on honor's terms. Their careers and marriages straddle both regimes' demands simultaneously.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, transitional_generation_elites, payer,
    powerful, biographical, trapped, national).

% Men inside honor society without the marksmanship, wealth, or standing to survive the challenge ritual — poor shots, junior officers, the indebted, the physically frail. Under honor axioms they could be maneuvered into accepting challenges they would likely lose or into refusals that destroyed them socially. Dignity axioms withdraw the compulsion; what flows to them is not a payment but the removal of a mortal hazard they could not previously decline.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, challenge_vulnerable_men, beneficiary,
    moderate, biographical, constrained, national).

% Lawyers, physicians, merchants, academics, and civil servants whose model of self-worth — intrinsic, inward, achievement-based rather than conferred by peer punctilio — the new axioms ratify as the definition of civilized personhood. They gain the normative high ground: their way of being a man becomes definitionally modern while the honor practitioner becomes definitionally archaic. Expanding economic opportunity gives them exits and security independent of any single patronage network.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_classes, beneficiary,
    organized, generational, mobile, continental).

% Clergy, evangelical reformers, sentimental novelists, and publicists who articulated and circulated the dignity axioms through sermons, reviews, and fiction. They collect cultural authority as their critique prevails — pulpits, periodicals, and academies open to them. They propagate rather than administer: the reading locates no enforcing office in their hands, only channels of transmission.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_moralists, beneficiary,
    organized, generational, mobile, continental).

% Women whose reputations served as collateral in male honor transactions — chastity functioning as the currency over which men dueled. Neither regime consulted them: honor culture spent their reputations as stakes, and dignity culture redefined respectability around them without their authorship. They would object to the terms of both settlements, and they sit outside the historiography's protagonist set until the gender-history revisions of the late twentieth century.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_of_honor_society, excluded,
    powerless, generational, trapped, national).

% Reconstruct the transition from duel statistics, conduct literature, court records, and memoirs; adjudicate between competing mechanism readings of the disappearance; hold no stake in either framework's survival and can see the full structure from outside both the honor and dignity grammars.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, bourgeois_professional_classes).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dignity culture coordinates membership in civilized society around a shared criterion of personhood: worth is intrinsic and unconditional rather than conferred by peer recognition and defended by combat. It solves a real de-escalation problem — strangers and rivals gain mutual assurance that insults will not escalate to lethal challenge, replacing honor's escalation protocol with a forbearance protocol.
% TRANSFER_FUNCTION: Moves normative authority and masculine status-legitimacy from honor-culture practitioners to dignity-culture adherents: what counts as an honorable man is redefined, and the honor framework's definitions lose official standing. Simultaneously moves physical security to all parties — the risk of socially coerced lethal combat is withdrawn — with the cost of that transfer borne by the honor practitioners whose meaning-system becomes illegible.
% ABSENT_VOICES: Women of honor society would object if present: both regimes disposed of their reputations without their consent — honor culture spent them as duel stakes, dignity culture redefined respectability over their heads. The duel dead are likewise absent: neither framework ever asked theirs or their families' consent to the wager their social world placed on their lives. Both voices sit outside the transition's recorded deliberations entirely.
% DISAPPEARANCE_RATIONALE: If the dignity-culture substrate vanished overnight, the rearrangement would be immediate and vast: assault law's grading of provocation, diplomatic insult protocols, school discipline, military justice, and everyday civility all presuppose dignity axioms. Honor logic would re-emerge wherever status is contested — professions, politics, domestic life — because the forbearance equilibrium depends on the shared axiom that no slight obligates lethal answer.
% FOUNDING_PROBLEM: The arrangement was articulated to solve the problem of ascribed, combat-backed worth: under honor culture, personal security hung on other men's punctilio and a man's life could be lawfully wagered on an insult. Enlightenment and evangelical moralists framed dignity as the replacement — worth made unconditional, security decoupled from the satisfaction ritual.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal historians and historical sociologists of the duel (the Kiernan-Frevert-Nye line of scholarship) attest from archival and statistical evidence that the anti-dueling problem is solved — challenge cultures collapsed and no serious party claims dueling remains a live social problem in dignity-culture societies. The assertion that a new live problem (incivility, bullying, dignity deficits) now sustains the arrangement comes only from within the beneficiary set — civility movements and their heirs — and is discounted accordingly per the corroboration rule.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim and the metrics are authored independently. The claimed_type is mountain because this reading's thesis is precisely substrate status: dignity axioms as an irreversible shift in what is thinkable, needing no administering office, no enforcement budget, and no defending coalition — hence emerges_naturally true, accessibility_collapse 0.88 (once inside dignity culture, the honor option is unavailable as a thought, not merely proscribed), and resistance 0.15 at interval end (the fight is over; nobody organizes against dignity culture qua substrate). The metrics describe the arrangement's actual operation as this reading assesses it: extractiveness 0.32 is low-to-moderate — the arrangement mostly subsidizes (withdrawn mortal hazard, ratified selfhood) but genuinely extracts from the honor residuals, whose inherited scripts became crimes or jokes; suppression 0.18 is low because unthinkability is self-enforcing and the honor machinery's coercive capacity decayed (traced in the falling suppression_requirement series — an enforcement-capacity story, which is why that series is authored); theater_ratio 0.12 is low because dignity culture is not maintained performatively. The extractiveness series peaks mid-interval (0.34-0.36 around 1830-1860) reflecting the transitional generation's double bind, then settles to the residual burden. All three series run on one shared six-point grid so no metric row borrows another's end-state values. Suppression is authored as a raw structural property; the engine scales only extractiveness by directionality and scope. Because a mountain here declares beneficiaries, the false-summit signature will evaluate this story — that evaluation is wanted data, not a defect: the reading itself is uncertain whether substrate status survives the presence of identifiable collectors, and omega natural_substrate_vs_constructed_order documents exactly that uncertainty.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute radically different types from identical structural data. From the honor practitioner's seat, the dignity order is annihilating: it did not defeat his framework, it made his framework impossible to inhabit intelligibly — his courage became pathology, his obligations became crimes, his death became an anecdote. From the bourgeois professional's seat, the same substrate is liberation and elevation: the withdrawal of a lethal hazard and the ratification of his own selfhood as the civilizational standard. From the transitional elite's seat it is a trap with two exits, both fatal. The engine computes these divergences from power, exit options, and directional position; the authored mountain claim does not adjudicate them — and the gap between the claim and the computed per-seat types is part of what this story exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Challenge_vulnerable_men and bourgeois_professional_classes sit near the beneficiary end: the arrangement withdraws hazards from the former and confers normative hegemony on the latter, and neither bears its costs. Dignity_culture_moralists derive low directionality as beneficiaries despite their propagator role — propagation is not administration, and the reading locates no enforcing office in their hands. Honor_culture_practitioners sit near the full-target end: they bear the extraction (framework illegibility), and their identity_locked exit pushes them further toward the target pole than their considerable power alone would — a powerful agent with no exit route is more fully targeted than a weak one with mobility. Transitional_generation_elites are trapped between regimes, the highest-directionality seat in the story. Women_of_honor_society are excluded rather than positioned: they feed the absent-voices record, not the directionality arithmetic. Notably, the story contains no agenda_setter — that absence is the reading's substantive claim, not an omission: dignity culture, on this account, has no administering seat, which is what distinguishes a substrate from a managed arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending ascribed, combat-backed worth and the coerced lethal wager — is dead: solved, corroborated by scholarship outside the beneficiary set. Yet the arrangement persists and deepens. The classification apparatus prevents two mislabelings here. Against the snare reading: dignity culture is not extraction wearing a coordination costume — its extraction is a residual tail, not its engine, and no seat administers it. Against the rope reading: there is no coordinating coalition maintaining it by choice; it behaves like terrain. The mountain claim captures the substrate thesis while the declared beneficiaries keep the false-summit question live. On the R5 mismatch: founding_problem_status dead combined with disappearance_verdict world_rearranges flags a completed mandate with a persisting arrangement — but the low theater_ratio (0.12) shows the persistence is not performative maintenance; it is substrate-deepening, which is exactly this reading's thesis. The mandatrophy is resolved in the specific sense that matters: the anti-dueling mandate finished its work, and what remains is not a zombie office but a changed grammar.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_substrate_vs_constructed_order,
    'Is dignity culture a genuine irreversible substrate (a mountain of cultural evolution) or a constructed normative order with identifiable beneficiaries and victims — a false summit that merely presents itself as natural law?',
    'Comparative analysis of attempted honor-code revivals (Weimar-era Mensur defenses, interwar nostalgia movements, surviving duel practices in Latin America) and cross-cultural variation in dignity-norm adoption; if revivals repeatedly fail across unrelated polities, substrate status strengthens; if revivals track enforceable interests, construction is indicated.',
    'Substrate confirmed: the mountain claim stands and the victim tail is read as transition cost. Constructed: the false-summit signature fires and the constraint recomputes as a hybrid with honor-culture practitioners as coordinated payers and the dignity-adherent classes as collectors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_substrate_vs_constructed_order, empirical, 'Whether the dignity-culture displacement is natural substrate or constructed order presenting as natural.').

omega_variable(
    reading_isolation_validity,
    'This constraint is one reading of the kernel dueling_disappearance_mechanism: does the axiomatic contraction mechanism operate independently of institutional substitution (courts, banking, libel law), or are the two mechanisms inseparable in the historical record such that this reading''s epsilon and victim set are contaminated by sibling mechanisms?',
    'Counterfactual comparison across polities: cases where dignity axioms diffused without strong institutional substitutes (dueling persistence in weak-court environments with strong dignity diffusion) versus cases of strong courts where honor obligations persisted; partial correlation of the two mechanisms'' timelines.',
    'If inseparable, this reading''s extractiveness and victim declarations partially measure the sibling''s mechanism and the classification holds only conditionally; if separable, the contraction reading stands as a clean epsilon-invariant constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_isolation_validity, conceptual, 'Whether the contraction mechanism can be isolated from institutional substitution in assigning epsilon and victims.').

omega_variable(
    victim_status_of_illegibility,
    'Are honor-culture practitioners genuinely victims bearing extraction, or merely losers of a normative competition whose loss is the ordinary price of moral progress — is ''framework illegibility'' a cost the standing arrangement imposes or simply a description of transition?',
    'Cohort-comparative biographical evidence: suicide rates, political radicalization, and memoir testimony of honor-socialized men across the transition, comparing cohorts formed before versus after the axiomatic shift; differential outcomes net of economic dislocation.',
    'If illegibility is not extraction, the victim set empties, false-summit pressure drops, and mountain certification strengthens; if it is extraction, the hybrid computation with honor practitioners as payers is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_illegibility, conceptual, 'Whether the honor practitioner''s loss constitutes imposed extraction or competitive defeat.').

omega_variable(
    irreversibility_testability,
    'Can the substrate-irreversibility claim be tested at all, or is it unfalsifiable narrative closure — what evidence would count as a genuine revival of honor-culture thinkability?',
    'Specify revival criteria in advance (widespread voluntary acceptance of lethal challenge obligations among elite males, judicial tolerance, conduct literature re-endorsing the satisfaction ritual) and audit near-miss episodes — Mensur survival, dueling survivals in France into the 1880s, Latin American challenge cultures — against them.',
    'Determines whether accessibility_collapse at 0.88 reflects a permanent substrate or a contingent equilibrium that policy or status competition could reopen; a reopenable equilibrium would push the classification away from mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_testability, empirical, 'Falsifiability of the irreversibility claim underlying the mountain typing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1760, 1914).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddm_contraction_tr_t1760, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1760, 0.05).
narrative_ontology:measurement(ddm_contraction_tr_t1795, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1795, 0.06).
narrative_ontology:measurement(ddm_contraction_tr_t1830, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1830, 0.08).
narrative_ontology:measurement(ddm_contraction_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(ddm_contraction_tr_t1890, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1890, 0.11).
narrative_ontology:measurement(ddm_contraction_tr_t1914, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1914, 0.12).

% Extraction over time
narrative_ontology:measurement(ddm_contraction_be_t1760, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1760, 0.12).
narrative_ontology:measurement(ddm_contraction_be_t1795, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1795, 0.2).
narrative_ontology:measurement(ddm_contraction_be_t1830, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1830, 0.34).
narrative_ontology:measurement(ddm_contraction_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.36).
narrative_ontology:measurement(ddm_contraction_be_t1890, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1890, 0.34).
narrative_ontology:measurement(ddm_contraction_be_t1914, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1914, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(ddm_contraction_su_t1760, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1760, 0.75).
narrative_ontology:measurement(ddm_contraction_su_t1795, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1795, 0.55).
narrative_ontology:measurement(ddm_contraction_su_t1830, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1830, 0.38).
narrative_ontology:measurement(ddm_contraction_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.28).
narrative_ontology:measurement(ddm_contraction_su_t1890, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1890, 0.22).
narrative_ontology:measurement(ddm_contraction_su_t1914, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1914, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'dueling disappeared' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-story constraint family. This story (contraction_reading) authors epsilon for the standing dignity-culture arrangement as seen from the contraction seat: the axiomatic displacement itself, with a residual extraction tail borne by honor practitioners whose framework became illegible. The sibling institutional_displacement_reading authors epsilon for a different referent — the courts/banking/libel-law arrangements that outcompeted the duel as dispute resolution — and would carry different beneficiaries (state judiciaries, commercial credit institutions) and different victims (debtors and libeled parties under the older regimes). The sibling overdetermined_composite_reading distributes causation across multiple sufficient conditions and authors epsilon over the joint arrangement. The upstream/downstream structure runs from this reading to the composite: the composite reading cites the documented axiomatic shift as one of its components, so this story's claim is load-bearing for the sibling's. All three files link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
