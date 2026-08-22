% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Backward Constraint with Extraordinary-Justification Departure Gate
 *   domain: legal/jurisprudential/constitutional
 *
 * SUMMARY:
 *   In the strict reading of the precedent corpus, decided holdings bind
 *   later courts as backward-looking constraints: a court confronting a
 *   governed dispute must apply the earlier holding, and departure is
 *   licensed only upon extraordinary justification — changed facts,
 *   demonstrated error of consequence, reliance evaporated. The arrangement
 *   is administered by the apex court, which controls both which challenges
 *   reach merits review and what counts as extraordinary. Genuine
 *   coordination runs through it: like cases are treated alike across
 *   decades, reliance interests are honored, decision costs fall, and judges
 *   are shielded from charges of case-by-case willfulness. Asymmetric
 *   extraction runs through the same structure: whoever won the leading
 *   holdings converts them into standing assets, while parties and movements
 *   whose claims require the corpus to change must lose repeatedly, across
 *   generations, before a door opens. FAMILY NOTE: the colloquial label
 *   'stare decisis' decomposes into structurally distinct readings of one
 *   kernel (common_law_precedent_corpus); this file instantiates ONLY the
 *   strict reading, with its own stable epsilon, beneficiaries, and victims.
 *   The evolutionary and pluralist readings are separate constraint files
 *   with different epsilon values and victim sets; they are linked, not
 *   averaged, and no reading's epsilon is hedged against another's.
 *   CLAIM/METRIC INDEPENDENCE: claimed_type is authored from structural
 *   assessment (genuine coordination plus asymmetric extraction under active
 *   enforcement); the metrics are authored from descriptive history; neither
 *   was tuned toward the other or toward a predicted engine output.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda-setter (institutional / identity_locked) — administers the corpus, controls which challenges are heard and what counts as extraordinary cause
 *   - incumbent_repeat_litigants: primary beneficiary (powerful / arbitrage) — converts settled holdings into standing assets; collects the predictability rents
 *   - adverse_precedent_litigants: primary target (moderate / trapped) — bears the binding force of holdings running against them
 *   - novel_rights_claimants: primary target (powerless / trapped) — narrowest challenge pathway; bears the generational closure
 *   - lower_court_judges: dual-positioned (institutional / constrained) — receives predictability, surrenders discretion
 *   - legislative_bodies: excluded seat (institutional / constrained) — displaced wherever holdings constitutionalize policy
 *   - legal_historians_and_commentators: analytical observer — sees the full structure, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.66).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.64).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.66).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Backward Constraint with Extraordinary-Justification Departure Gate").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'ef453523-d6a9-4a6b-8143-b5fd0ff2d492').
narrative_ontology:cs_kernel_codification('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', formalized).
narrative_ontology:cs_authority_grounding('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', extraction).
narrative_ontology:cs_interpretation_layer_present('ef453523-d6a9-4a6b-8143-b5fd0ff2d492').
narrative_ontology:cs_reading_relation('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', common_law_precedent_corpus__evolutionary_framework, forecloses).
narrative_ontology:cs_reading_relation('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', common_law_precedent_corpus__pluralist_balancing, forecloses).
narrative_ontology:cs_axiom('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', foundational, precedent_binding_is_default_rule).
narrative_ontology:cs_axiom_status(precedent_binding_is_default_rule, holdable).
narrative_ontology:cs_axiom_grounding('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', precedent_binding_is_default_rule, conventional).
narrative_ontology:cs_axiom('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', foundational, departure_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(departure_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', departure_requires_extraordinary_justification, conventional).
narrative_ontology:cs_reference_frame('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', accumulated_holdings_as_binding_law).
narrative_ontology:cs_drift_state('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', contemporary_post_dobbs, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef453523-d6a9-4a6b-8143-b5fd0ff2d492', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, incumbent_repeat_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, supreme_court_justices).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, adverse_precedent_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, novel_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_predictability).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, equal_treatment_of_like_cases).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_legitimacy_through_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured judges who decide which holdings govern, which challenges reach full merits review, and whether any precedent is overruled. They administer a corpus they wrote or inherited, and their public legitimacy rests on presenting outcomes as compelled by settled law rather than chosen. The only exit from the role is leaving the bench, and the role's authority is constituted by the fidelity practice itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Corporations, trade associations, and government agencies that won the leading holdings or operate under them. Each settled rule converts past victories into standing assets: opponents must push challenges through a narrow reopening path while they defend on familiar ground. They choose when to press an issue, which forums to use, and which challenges to settle away, and they fund the doctrinal defense of the holdings they rely on.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, incumbent_repeat_litigants, beneficiary,
    powerful, generational, arbitrage, national).

% Parties whose cases fall under holdings running against them. They may argue facts, distinguish at the margins, or seek en banc or certiorari review, but the holding stands until the apex court grants review and accepts extraordinary cause; most challenges die at the discretionary-review stage. Their litigation spending buys incremental narrowing at best.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, adverse_precedent_litigants, payer,
    moderate, biographical, trapped, national).

% Groups seeking recognition the corpus does not yet confer — historically, enslaved and formerly enslaved people, married women, criminal defendants, same-sex couples. Until the apex court revisits a line of holdings, their claims fail regardless of present-day support; their remedy is to lose repeatedly while building the record a future court might accept. Generations pass before the door opens, and each loss is cited as proof the question is settled.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, novel_rights_claimants, payer,
    powerless, generational, trapped, national).

% District and circuit judges receive a ready-made rule for most disputes, which clears dockets and shields them from accusations of personal whim. The same rule removes their discretion whenever they believe a holding is wrong: they must apply it, write opinions explaining why it controls, and face summary reversal if they strain against it.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges, payer).

% Where holdings constitutionalize a policy question, legislatures are displaced from it: statutes conflict with a ruling they cannot vote to amend, and the remaining remedies are confirming new justices or constitutional amendment. On statutory holdings they retain override power, but on constitutional ones they deliberate outside the room where the rule is made.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% Scholars who track the corpus's growth, measure overruling rates, and document the gap between fidelity rhetoric and practice. They publish critiques, propose departure standards, and supply the historical record on which contestants draw, but hold no vote in the courtroom.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_historians_and_commentators, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, incumbent_repeat_litigants).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurring coordination problem of a multi-tribunal, multi-generation legal system: thousands of disputes must be resolved consistently without renegotiating first principles case-by-case. Settled holdings give lower courts ready rules, give parties calculable positions, protect reliance built on yesterday's law, and reduce the decision costs and political exposure of judging.
% TRANSFER_FUNCTION: Moves outcome-determinative authority from present litigants and present majorities to past courts' holdings and to the parties positioned under them; moves litigation costs onto challengers, who must build extraordinary records to reopen anything; delivers predictability rents to repeat players holding favorable doctrine and legitimacy rents to the administering court.
% ABSENT_VOICES: Legislative bodies displaced where holdings constitutionalize policy questions — they deliberate outside the room where the rule is made and can respond only through appointments or amendment. Also absent: the losing parties in the founding cases whose defeat was converted into perpetual rule without their consent to perpetuity; present-day citizens whose considered judgments are frozen inside constitutionalized holdings; and the generations who will inherit today's compromises hardened into tomorrow's starting positions.
% DISAPPEARANCE_RATIONALE: If backward binding vanished overnight, every governed dispute would reopen first principles: lower courts would re-decide settled questions, reliance built on holdings would destabilize, repeat players would lose their standing assets, and appellate dockets would flood. The common-law system would reorganize around case-by-case reevaluation, and the apex court's agenda control would become the only remaining ordering device.
% FOUNDING_PROBLEM: The early republic inherited scattered, conflicting state and federal decisions with no unified method: like cases came out differently across circuits, property and contract expectations were unstable, and judges faced charges of personal whim. The arrangement was built to unify the law, stabilize reliance, cut decision costs, and present judicial outcomes as compelled by settled rule rather than chosen.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legal historians document the pre-unification fragmentation of the early-national judiciary; nineteenth-century treatise writers such as Story and Kent attested the coordination deficit they sought to cure; comparative jurisprudence shows parallel coordination needs producing analogous doctrine across common-law systems; and the arrangement's sharpest critics — legal realists and critical scholars — corroborate that the coordination functions are real while disputing the strictness of the departure gate. No corroborating source attests that the strict threshold itself was demanded by the founding problem; that increment is this reading's own contribution.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66 and suppression 0.64 are measured at the interval's contemporary end (post-Dobbs consolidation), the current phase of a documented cycle: extraction peaks when the corpus entrenches one coalition's victories (Plessy 1896, Lochner 1905: 0.68-0.74), collapses when political realignment forces yield (1937: 0.52; Warren era: 0.44), and re-accumulates as the new settlement hardens (1992-2025: 0.63-0.66). The oscillation is partly an extraction mechanism in itself: each favorable phase consolidates holdings, and the extraordinary-justification gate then locks the consolidation against the next coalition's challengers — a ratchet, not noise. Theater_ratio 0.48 sits just under the substitution line: fidelity rhetoric is increasingly performative (reaffirm-while-narrowing opinions; extended stare decisis discussions concluding in overruling) while the operative machinery — certiorari gatekeeping, hierarchical reversal, narrowing-by-distinction — remains functional. Accessibility_collapse 0.55: understood alternatives (overruling petitions, constitutional amendment, legislative override of statutory holdings, persuasive foreign authority) persist but are narrow and slow. Resistance 0.58: persistent dissents, academic campaigns, repeated challenger litigation, court-reform movements. Suppression is authored as a raw structural property and is deliberately NOT scaled — the engine scales only extractiveness, by directionality and scope. Coalition note: the powerless payer seat (novel_rights_claimants) exercises influence only through multi-generation coalition litigation that builds the record a future court will accept — the movement-litigation pattern — which is resistance operating on decade timescales, not exit.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the judiciary's own legitimacy: outcomes presented as compelled rather than chosen, decision costs deferred to settled rules, political heat absorbed by the claim that the law compelled the result. From the trapped payer seats the identical structure is closure: a holding against you ends the argument regardless of present merit, and the reopening path is gated by the very institution that issued the holding. Lower-court judges occupy both positions at once — predictability received, discretion surrendered. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (incumbent_repeat_litigants, supreme_court_justices, lower_court_judges) drive those seats toward the beneficiary end of d; victim declarations (adverse_precedent_litigants, novel_rights_claimants) drive those seats toward the target end, amplified by trapped exit and, for the claimants, powerless power. No directionality_overrides are authored: the derivation distinguishes the three institutional seats through their differing role declarations (agenda_setter; beneficiary; beneficiary carrying a secondary payer position), and the one candidate correction — the agenda-setter's own subjection to horizontal binding — is carried by that dual positioning rather than by a power-atom override, which would misfire across the three distinct institutional seats sharing the institutional atom. Vindicated propositions (rule_of_law_predictability, equal_treatment_of_like_cases, judicial_legitimacy_through_continuity) are listed as vindicated_propositions, never as beneficiaries: doctrines collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — unifying scattered decisions, protecting reliance, cutting decision costs, insulating judging from politics — remain live, so the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no dead-mandate flag. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure coordination would erase the documented generational closure imposed on challenger classes; reading it as pure extraction would erase the real reliance and consistency functions that even its victims invoke when the holdings favor them. Mandatrophy is not declared: the mandate has not outlived its function, though its strictness — the specific claim that departure requires extraordinary justification — is the contested element, and that contest is routed to the committer omega rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the precedent-corpus kernel governs the operative rule: this story''s strict-binding instantiation, or one of the sibling readings?',
    'Framework adoption is resolved by which rule of decision the apex court announces and applies — track the stated departure standard in overruling and denial opinions over time.',
    'Under evolutionary_framework the victim set contracts (novel claimants gain a default reopening path) and epsilon falls; under pluralist_balancing epsilon fragments by domain and the uniform-binding structure dissolves into per-domain arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading (strict_stare_decisis) of the common_law_precedent_corpus kernel; siblings are separate constraints, and the disagreement lives in the departure-licensing rule.').

omega_variable(
    coordination_rent_attribution,
    'How much of the measured extraction is the inherent price of predictability and reliance protection, versus rent accruing to holders of favorable precedent?',
    'Compare reliance-disruption outcomes after major departures (Brown 1954, Dobbs 2022) against predicted collapse; measure repeat-player win-rate differentials under settled versus unsettled doctrine.',
    'If most extraction is coordination cost, the arrangement trends toward pure coordination; if rent dominates, it trends toward the extractive pole and the extraordinary-justification gate reads as entrenchment machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_rent_attribution, empirical, 'Separating coordination cost from incumbent rent within the measured extraction.').

omega_variable(
    rigidity_endogeneity_cert_gate,
    'Is the observed rarity of departure a property of the binding rule itself, or of discretionary-review gatekeeping that decides which challenges are heard at all?',
    'Compare overruling rates and grant rates for precedent-challenges across certiorari regimes — the pre-1925 mandatory appeal era versus the modern fully discretionary era.',
    'If gatekeeping drives rigidity, the suppressive force belongs to the agenda-control apparatus rather than the doctrine, and reforms targeting the gate would loosen the arrangement without touching the rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_endogeneity_cert_gate, empirical, 'Whether doctrinal rigidity is intrinsic or an artifact of agenda control.').

omega_variable(
    judicial_adherence_internalization,
    'Is judicial adherence to settled holdings structural (reversal risk, hierarchical discipline) or internalized (professional identity constituted by fidelity)?',
    'Post-retirement behavior of former judges: public criticism of holdings they enforced, dissent-pattern interviews, and voting comparisons before and after leaving the hierarchy.',
    'If internalized, effective suppression exceeds the structural measure and persists even where enforcement slackens; the scalar suppression decomposes into structural and identity components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_adherence_internalization, empirical, 'Structural versus internalized component of judicial adherence to the corpus.').

omega_variable(
    stability_access_weighting,
    'How should the arrangement weigh stability-for-those-protected-by-settled-holdings against access-for-those-whose-claims-the-holdings-close — is the extraordinary-justification threshold set at the right height?',
    'Not resolvable by data alone; resolved by the polity''s choice among rule-of-law conceptions, expressed through appointments, jurisdiction-stripping, or amendment.',
    'A polity prioritizing corrective access would lower the threshold (trending toward the pluralist reading); one prioritizing reliance would raise it further; the arrangement''s normative valence flips with the choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_access_weighting, preference, 'Value weighting between reliance stability and corrective access in setting the departure threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_strict_sd_tr_t1800, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1800, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1857, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1857, 0.25).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1857, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1896, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1896, 0.22).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1896, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1905, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1905, 0.28).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1905, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1937, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1937, 0.48).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1937, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1954, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1954, 0.38).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1954, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t1992, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1992, 0.45).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t1992, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t2022, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2022, 0.46).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t2022, observed).
narrative_ontology:measurement(clpc_strict_sd_tr_t2025, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(clpc_strict_sd_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clpc_strict_sd_be_t1800, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1800, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1857, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1857, 0.56).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1857, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1896, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1896, 0.68).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1896, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1905, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1905, 0.74).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1905, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1937, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1937, 0.52).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1937, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1954, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1954, 0.44).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1954, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t1992, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1992, 0.63).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t1992, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t2022, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t2022, observed).
narrative_ontology:measurement(clpc_strict_sd_be_t2025, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(clpc_strict_sd_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clpc_strict_sd_su_t1800, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1800, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1857, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1857, 0.4).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1857, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1896, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1896, 0.5).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1896, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1905, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1905, 0.58).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1905, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1937, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1937, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1954, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1954, 0.33).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1954, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t1992, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1992, 0.6).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t1992, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t2022, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2022, 0.61).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t2022, observed).
narrative_ontology:measurement(clpc_strict_sd_su_t2025, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(clpc_strict_sd_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'stare decisis' label per the epsilon-invariance principle: the colloquial concept covers at least three structurally distinct claims about how the precedent corpus binds. This file carries the strict reading (uniform backward binding, extraordinary-justification gate, high rigidity, challenger-closed victim set, epsilon 0.66). The evolutionary reading carries a lower epsilon with a contracted victim set; the pluralist reading fragments epsilon by domain. The strict reading is the historical baseline from which the others define themselves; each family file links the others here, and no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
