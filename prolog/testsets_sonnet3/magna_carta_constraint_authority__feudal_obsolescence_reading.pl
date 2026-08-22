% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Feudal Obsolescence Reading of Magna Carta's Constraint Authority
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal-obsolescence reading of the Magna
 *   Carta constraint-authority kernel: the claim that the 1215 charter was
 *   strictly a baronial settlement of 13th-century feudal grievances,
 *   annulled almost immediately and diluted across subsequent reissues, and
 *   therefore carries no binding authority over modern sovereignty or
 *   due-process structures. Structurally, this reading functions less as
 *   neutral historiography and more as an argumentative resource: in the
 *   hands of executive and security actors, the 'merely feudal, merely
 *   historical' framing is deployed to foreclose due-process arguments
 *   grounded in the charter's lineage, without those actors having to contest
 *   the doctrinal merits directly. The reading rides on genuinely accurate
 *   historical scholarship (the charter WAS narrow, WAS annulled, WAS
 *   diluted) but extends that accuracy into a contested inferential leap
 *   (therefore no modern constraint authority survives) that primarily serves
 *   parties who benefit from maximized executive discretion. This is why the
 *   type is authored as piton rather than mountain: what began as (or could
 *   function as) a correct, low-extraction historical clarification has, in
 *   practice, atrophied into a rhetorical tool wielded selectively wherever
 *   it serves discretion-maximizing outcomes, while the underlying historical
 *   scholarship itself is not in serious dispute. Sibling readings
 *   (living_constitutionalism_reading, parliamentary_sovereignty_reading) are
 *   NOT part of this constraint; they are separate files linked via
 *   network.affects_constraints, each with its own ε and stakeholder
 *   structure, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - executive_branch_officials: institutional beneficiary who deploys the reading to expand discretion
 *   - national_security_apparatus: institutional beneficiary/agenda_setter sustaining the reading's circulation in litigation
 *   - popular_constitutionalism_advocates: moderate-power payer whose living-precedent arguments are foreclosed
 *   - juridical_restraint_proponents: organized payer bearing doctrinal-labor costs
 *   - habeas_corpus_petitioners: powerless, trapped payer bearing the reading's practical consequences
 *   - constitutional_historians: analytical observer corroborating the narrow facts but not the inferential leap
 *   - legislature: excluded institutional actor whose codification role is foreclosed by treating the restraint as already dead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Feudal Obsolescence Reading of Magna Carta's Constraint Authority").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '912b964b-0b59-4ca2-971d-7fdbcb7e9a13').
narrative_ontology:cs_kernel_codification('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', fixed_text).
narrative_ontology:cs_authority_grounding('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', extraction).
narrative_ontology:cs_interpretation_layer_present('912b964b-0b59-4ca2-971d-7fdbcb7e9a13').
narrative_ontology:cs_reading_relation('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', foundational, origin_scope_binds_descent).
narrative_ontology:cs_axiom_status(origin_scope_binds_descent, holdable).
narrative_ontology:cs_axiom_grounding('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', origin_scope_binds_descent, empirically_contingent).
narrative_ontology:cs_axiom('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', secondary, no_doctrinal_authority_without_continuous_original_intent).
narrative_ontology:cs_axiom_status(no_doctrinal_authority_without_continuous_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', no_doctrinal_authority_without_continuous_original_intent, conventional).
narrative_ontology:cs_reference_frame('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', narrow_baronial_settlement_1215).
narrative_ontology:cs_drift_state('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', contemporary_executive_power_disputes, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('912b964b-0b59-4ca2-971d-7fdbcb7e9a13', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch_officials).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, habeas_corpus_petitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the feudal-obsolescence reading to argue that Magna Carta's due-process and lawful-judgment clauses were negotiated by barons for barons in a vanished feudal order, and therefore impose no binding restraint on contemporary executive action. This clears the field for expanded discretion in detention, surveillance, and emergency powers without having to argue the merits of restraint directly — the argument is that there was never a live constraint to begin with.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch_officials, beneficiary,
    institutional, biographical, arbitrage, national).

% Relies on the obsolescence framing in litigation and policy memoranda to resist claims that historic due-process guarantees constrain modern detention or search practices. Actively cites the 13th-century specificity of the charter's grievances (scutage, forest law, baronial councils) to characterize any modern invocation of Magna Carta as anachronistic overreach by advocates, and helps sustain the reading's circulation in legal argument.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus, agenda_setter).

% Argue that Magna Carta's core commitments — no punishment without lawful judgment, restraint on arbitrary seizure — were never merely feudal but articulated a principle that later constitutional orders inherited and built upon. Under the obsolescence reading, this argument is treated as sentimental or historically naive, foreclosing its use as living precedent in court and public debate. They cannot force the reading's abandonment through litigation alone; the reading persists in legal education and judicial opinion regardless of their objections.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    moderate, generational, constrained, national).

% Judges, bar associations, and legal scholars who hold that the rule-of-law principle in clause 39/40 evolved through centuries of common-law elaboration into modern due process. The feudal-obsolescence reading, when adopted by courts, strips their citations of Magna Carta of persuasive weight, forcing them to rebuild the same restraint arguments from later, more contestable sources — a real cost in doctrinal labor and litigation outcomes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents, payer,
    organized, generational, constrained, national).

% Individuals detained by the state who might invoke Magna Carta's lineage as rhetorical or doctrinal support for release. Under the obsolescence reading, courts treat such invocations as historically inert, removing one available argumentative resource precisely when they have the fewest others. They bear the reading's practical consequences without having any voice in which reading a court adopts.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, habeas_corpus_petitioners, payer,
    powerless, immediate, trapped, national).

% Study the actual 13th-century context of the 1215 charter — its baronial self-interest, its near-immediate annulment by the Pope, its reissues under Henry III — and can attest that the document's original function was narrow and feudal-specific. They can also attest that later generations reinterpreted and repurposed clauses for different ends, without settling which reading should govern modern constitutional argument.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% Would be the body that could, under a parliamentary-sovereignty framing, explicitly absorb or revise Magna Carta's restraints into statute — but the feudal-obsolescence reading forecloses that conversation by treating the underlying restraint as already dead rather than as live material awaiting legislative codification or repeal. Legislature's potential role in adjudicating the charter's ongoing relevance is not engaged under this reading.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, legislature, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, national_security_apparatus).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the present tense — the reading's stated function is historical accuracy: correctly locating Magna Carta as an artifact of feudal baronial politics rather than as a source of binding modern restraint. This genuinely coordinates careful historical scholarship against anachronistic misreading.
% TRANSFER_FUNCTION: Moves argumentative and doctrinal weight away from due-process claimants and toward executive and security-apparatus discretion: what is transferred is not money but the availability of a historically grounded restraint argument, withdrawn from petitioners and juridical-restraint advocates and effectively ceded to whichever actor benefits from unconstrained executive action in a given case.
% ABSENT_VOICES: Habeas corpus petitioners and other individuals subject to detention or executive action have no forum in which to contest which historical reading of Magna Carta a court adopts; the choice of reading is made by judges and litigators arguing doctrine, while the people whose liberty depends on the outcome are structurally absent from that argument.
% DISAPPEARANCE_RATIONALE: If the feudal-obsolescence reading vanished overnight, executive and security actors would lose one argumentative resource for resisting due-process claims grounded in Magna Carta's lineage, and courts would need to independently evaluate its precedential weight rather than dismissing it as anachronistic; whether this meaningfully changes outcomes is disputed, since much modern due-process doctrine now rests on later sources (statute, later case law) that would still function without Magna Carta's rhetorical backing. Advocates say it would matter; skeptics say the modern doctrine no longer needs the charter regardless of which reading prevails.
% FOUNDING_PROBLEM: The reading was built to correct a genuine historiographic error: the romantic 19th- and 20th-century habit of treating Magna Carta as a proto-democratic bill of rights, when the actual 1215 document was a narrow settlement of baronial grievances against King John, annulled within weeks by Pope Innocent III, and reissued in progressively diluted forms that dropped most of its substantive content.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside any interested party corroborate the narrow historical facts (feudal specificity, papal annulment, dilution across reissues). However, the same historians do NOT corroborate the further inference the reading draws from those facts — that the charter therefore has no legitimate bearing on modern constraint doctrine; many historians hold the facts are compatible with genuine doctrinal descent through later common-law elaboration. The inferential leap from 'historically narrow origin' to 'no binding modern authority' is corroborated only by the beneficiary parties (executive and security actors), not by the historians whose scholarship the reading cites.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate-to-high (0.68 at T=100) because the reading's function is not merely descriptive: it is repeatedly invoked to foreclose a specific class of due-process argument, transferring argumentative leverage to executive and security actors. Theater ratio is authored high and rising (0.72) because the reading increasingly does more work as a rhetorical device deployed selectively in litigation than as a considered historiographic conclusion applied consistently — courts and advocates invoke it when convenient for discretion-maximizing outcomes and are comparatively silent on it elsewhere. Suppression (0.58) reflects that alternatives (the living-constitutionalism and parliamentary-sovereignty readings) are not eliminated but are argumentatively disadvantaged whenever this reading prevails in a given forum. Accessibility collapse is moderate (0.40) — the sibling readings remain fully articulable and are actively defended by historians and juridical-restraint proponents, so alternatives have not collapsed the way they would under a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive and security-apparatus stakeholders sit near the beneficiary end: they collect the practical benefit (expanded discretion) of treating the charter as historically inert, and their exit options (arbitrage — they can simply not invoke the charter's authority when it doesn't suit them, or invoke the obsolescence reading when it does) reinforce a low-d position. Popular constitutionalism advocates and juridical restraint proponents sit toward the target end: they bear the cost of losing a doctrinal resource, and their exit options are constrained — they cannot simply relitigate historical fact, only continue arguing doctrine in an environment where one live thread has been argued closed. Habeas corpus petitioners sit furthest toward full-target: trapped exit options, immediate time horizon, and no capacity to influence which reading a court adopts in their own case.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton (rather than mountain or snare outright) captures a specific structural fact: the founding historiographic correction (Magna Carta was not a proto-democratic charter) is genuinely settled and uncontested among historians — the founding problem in the narrow historical sense is resolved and, in that sense, 'dead' as a live scholarly dispute. But the reading's institutional life has NOT died with its founding problem; it persists and has hardened into selective invocation whose actual current function is discretion-maximization rather than historical correction. This is exactly the mismatch the founding_problem/disappearance_verdict pairing is designed to surface: founding_problem_status is authored 'contested' rather than flatly 'dead' because the narrow historical claim is settled while the derived doctrinal claim (no modern authority whatsoever) remains actively disputed and actively deployed — a zombie inference riding on a settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_vs_doctrinal_weaponization,
    'Is the feudal-obsolescence reading functioning primarily as accurate historiography that happens to be cited, or has it become a selectively-deployed argumentative tool whose primary current function is discretion-maximization for executive and security actors?',
    'A systematic study of citation patterns: does the reading appear symmetrically across cases regardless of outcome, or does it appear disproportionately in cases where its adoption favors expanded executive discretion? Track which party invokes the reading and in what procedural posture across a large sample of relevant litigation.',
    'If citation is symmetric and outcome-independent, the reading functions closer to genuine historiography (lower extraction, closer to mountain-with-declared-beneficiaries/FSM territory). If citation is asymmetric and outcome-correlated with executive/security interests, the piton or snare classification is strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_vs_doctrinal_weaponization, empirical, 'Whether the reading''s deployment pattern reveals rhetorical weaponization versus neutral historical citation.').

omega_variable(
    settled_fact_vs_contested_inference,
    'Is the disagreement between this reading and its siblings actually about historical fact (which is largely settled), or is it about a non-historical normative/legal-theoretical question (whether doctrinal descent through later elaboration can confer binding authority independent of an origin''s narrow scope) that historical facts alone cannot resolve?',
    'Careful separation, in legal scholarship and judicial opinion, of the historical claims (uncontested) from the jurisprudential claims about how common-law doctrine acquires binding force through elaboration and precedent (genuinely contested, a matter of legal theory not history).',
    'If the disagreement is purely jurisprudential rather than historical, the feudal-obsolescence reading''s use of settled history to imply a settled doctrinal conclusion becomes a specific and identifiable overreach — the reading would be exploiting agreement on facts to manufacture the appearance of agreement on a much more contestable normative question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settled_fact_vs_contested_inference, conceptual, 'Whether the reading conflates settled historical fact with a distinct, unsettled jurisprudential question about doctrinal descent.').

omega_variable(
    reading_selection_under_determination,
    'Could this constraint be authored equally well under the parliamentary_sovereignty_reading''s framing (Magna Carta''s restraints survive via absorption into statute, so the feudal-obsolescence claim is simply irrelevant to modern practice rather than actively extractive)? What signals led to authoring this as an active piton/snare rather than as a comparatively inert non-issue?',
    'Examine whether courts and litigators, when they invoke the obsolescence framing, are doing so in contexts where parliamentary/statutory absorption has already occurred (making the reading''s practical stakes low) or in contexts where no statutory absorption exists and the charter''s lineage is the only available argument (making the reading''s practical stakes high).',
    'If most relevant modern due-process protections have already been fully absorbed into statute, this reading''s practical extraction is lower than authored here (closer to a piton with negligible current stakes). If significant due-process argument still depends on unabsorbed constitutional lineage, this reading''s extraction is as significant as authored, or higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Alternative framing under which this reading''s practical stakes would be substantially lower, and what evidence would distinguish the two framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(magn_tr_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(magn_tr_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 60, 0.62).
narrative_ontology:measurement(magn_tr_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 80, 0.68).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 100, 0.72).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(magn_be_t40, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(magn_be_t60, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(magn_be_t80, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 100, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_constraint_authority__feudal_obsolescence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the magna_carta_constraint_authority kernel. The living_constitutionalism_reading and parliamentary_sovereignty_reading are separate constraint files, each with independently authored ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — this file does not average over or hedge against those readings; it authors this reading cleanly and routes the inter-reading contest to omega variables and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
