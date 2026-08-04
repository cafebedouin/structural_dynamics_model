% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Code Redefinition Excluding Dueling (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology
 *
 * SUMMARY:
 *   This story instantiates the contraction reading of the
 *   honor-violence-legitimacy kernel: as bourgeois, clerical, and state
 *   actors redefined 'honor' to mean interior virtue, legal probity, and
 *   self-restraint rather than willingness to answer insult with violence,
 *   dueling did not merely become risky or rare — it exited the conceptual
 *   space of legitimate honor responses altogether. This is structurally
 *   distinct from the drop reading (which holds dueling stayed legitimate but
 *   grew practically costly) and the composite reading (which holds both
 *   mechanisms operated together). Under the contraction reading, the
 *   decisive causal work is done by redefinition of the honor vocabulary
 *   itself, not by escalating external sanction. The suppression_requirement
 *   series therefore falls over time (as fewer duelists exist who need
 *   suppressing, because fewer people even conceive of dueling as honorable),
 *   while theater_ratio rises (performative moral condemnation of dueling
 *   increases even as actual duels become rare, since there is less genuine
 *   threat left to counter).
 *
 * KEY AGENTS:
 *   - bourgeois_professional_class: primary beneficiary of the redefinition — inherits elite status without needing violence
 *   - state_judicial_monopoly: agenda-setter administering the redefinition through law and prosecution
 *   - clergy_and_moral_reformers: co-agenda-setters who supply the moral vocabulary
 *   - declining_aristocratic_honor_culture: primary payer — loses the very vocabulary in which its status claims were legible
 *   - dueling_specialists_and_seconds: secondary payer — occupational extinction
 *   - legal_historians: analytical observer adjudicating between the three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Code Redefinition Excluding Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '4a788831-ed07-49b2-828d-0707ba3d5f17').
narrative_ontology:cs_kernel_codification('4a788831-ed07-49b2-828d-0707ba3d5f17', distributed).
narrative_ontology:cs_authority_grounding('4a788831-ed07-49b2-828d-0707ba3d5f17', distributed).
narrative_ontology:cs_reading_relation('4a788831-ed07-49b2-828d-0707ba3d5f17', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a788831-ed07-49b2-828d-0707ba3d5f17', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('4a788831-ed07-49b2-828d-0707ba3d5f17', foundational, honor_is_interior_virtue_not_combat_readiness).
narrative_ontology:cs_axiom_status(honor_is_interior_virtue_not_combat_readiness, holdable).
narrative_ontology:cs_axiom_grounding('4a788831-ed07-49b2-828d-0707ba3d5f17', honor_is_interior_virtue_not_combat_readiness, conventional).
narrative_ontology:cs_axiom('4a788831-ed07-49b2-828d-0707ba3d5f17', secondary, violent_vindication_exits_legitimate_honor_repertoire).
narrative_ontology:cs_axiom_status(violent_vindication_exits_legitimate_honor_repertoire, holdable).
narrative_ontology:cs_axiom_grounding('4a788831-ed07-49b2-828d-0707ba3d5f17', violent_vindication_exits_legitimate_honor_repertoire, conventional).
narrative_ontology:cs_reference_frame('4a788831-ed07-49b2-828d-0707ba3d5f17', aristocratic_martial_honor_code).
narrative_ontology:cs_drift_state('4a788831-ed07-49b2-828d-0707ba3d5f17', post_enlightenment_bourgeois_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('4a788831-ed07-49b2-828d-0707ba3d5f17', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_judicial_monopoly).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, declining_aristocratic_honor_culture).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, dueling_specialists_and_seconds).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_as_moral_interiority_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professionals (lawyers, merchants, civil servants) whose status depends on reputation for probity, contract-keeping, and self-restraint rather than martial prowess. The redefinition of honor as internal virtue and reliability, rather than willingness to kill or die over insult, converts their existing dispositions into the new elite currency without requiring them to acquire aristocratic combat skill or risk.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, and monarchical/republican authority structures actively redefine what counts as honorable response through statute, sermon, press campaign, and prosecution, reclassifying dueling as murder or assault rather than a parallel private-justice system. They administer the redefinition and benefit from it directly by consolidating the monopoly on legitimate violence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_judicial_monopoly, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Religious authorities and Enlightenment moralists campaign to relocate honor from external combat to internal virtue, producing sermons, tracts, and social pressure that makes dueling appear barbaric or theologically illegitimate rather than merely costly. Their cultural authority is enhanced as the arbiters of the new honor vocabulary.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers, agenda_setter).

% The traditional nobility whose status was historically constituted through willingness to duel finds that the entire vocabulary in which their honor claims were once legible has been redefined out from under them. They cannot simply keep dueling and remain honorable in the new discourse — the exit is not forbidden by force alone but has become unintelligible as a strategy for claiming status.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, declining_aristocratic_honor_culture, payer,
    powerful, biographical, constrained, national).

% Fencing masters, professional seconds, and dueling-code arbiters whose livelihood and social function depended on dueling's legitimacy lose their occupational niche entirely as the practice becomes conceptually incoherent rather than merely risky. There is no adjacent role for them within the new honor framework.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_specialists_and_seconds, payer,
    moderate, biographical, trapped, regional).

% Wives, daughters, and dependents of duelists bore the practical costs of the old honor system (widowhood, family ruin from feuds) but had no voice in either the old code or its redefinition. Their interests are invoked rhetorically by reformers but they are not parties to the negotiation over what honor means.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_and_excluded_dependents, excluded,
    powerless, biographical, trapped, national).

% Scholars reconstructing the transition read court records, honor-code manuals, and sermon literature to adjudicate whether dueling's decline reflects genuine conceptual redefinition, mere cost escalation, or both operating together. Their classification affects how later normative traditions describe honor's history.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable vocabulary for what counts as honorable conduct so that reputation, trust, and status claims can be verified and enforced without recourse to private violence — a genuine coordination problem in any status economy.
% TRANSFER_FUNCTION: Moves social and moral capital from the aristocratic honor economy (built on demonstrated willingness to fight) to the bourgeois-clerical honor economy (built on demonstrated interior virtue and legal compliance), while transferring the state's monopoly claim over legitimate force away from private codes of combat.
% ABSENT_VOICES: Women and dependents who bore the costs of the old system (and would bear different costs under either honor regime) are rhetorically cited by reformers but never seated at the table where honor's redefinition is negotiated; dueling specialists whose occupational identity is erased have no institutional voice either.
% DISAPPEARANCE_RATIONALE: If the conceptual redefinition had not occurred — if 'honor' retained its old meaning tied to violent vindication — the practical costs (legal penalties, social sanction against duelists) would have operated on a population that still recognized dueling as the correct response to insult, producing chronic tension between law and legitimate self-conception rather than dueling's disappearance from the space of thinkable responses. The redefinition itself is what makes disappearance stick rather than merely suppressed.
% FOUNDING_PROBLEM: Honor systems solve a real coordination problem: they let people signal trustworthiness, deter insult, and resolve disputes over reputation without appeal to a (historically weak or partisan) state. Dueling was one solution among possible solutions to 'how do we adjudicate honor claims.'
% FOUNDING_PROBLEM_CORROBORATION: Contemporary legal historians and comparative anthropologists (outside both the aristocratic and bourgeois beneficiary groups) attest that the underlying coordination problem — how reputational disputes get adjudicated without private violence — persisted and was resolved by substitute mechanisms (libel law, dueling's disappearance without an equivalent replacement for some functions, professional codes of conduct); the aristocratic beneficiaries of the old system are extinct as an interest group and cannot dispute the characterization, which limits independent corroboration of their side.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) because the contraction reading describes a genuine conceptual/cultural shift with real coordination benefits (reduced private violence, professionalized dispute resolution) rather than a naked transfer mechanism; some extraction exists because the redefinition was actively steered by beneficiary groups (bourgeoisie, clergy, state) who profited from the aristocracy's vocabulary being stripped of its former force. Suppression is moderate and falling — early in the interval, active suppression of dueling still matters because the old vocabulary has residual force; by the late interval, suppression matters less because almost no one conceives of dueling as honorable in the first place, so there is little residual behavior needing forceful suppression. Theater ratio rises because condemnation of dueling becomes increasingly performative as an actual threat: moralizing rhetoric intensifies in inverse proportion to the real incidence of the practice it condemns.
 *
 * PERSPECTIVAL GAP:
 *   From the state/clergy/bourgeois seat, this looks like coordination succeeding: honor becomes legible, dispute resolution centralizes, private violence declines — a rope story. From the declining aristocratic seat, the same event is extraction of status capital through unilateral redefinition of the terms by which status is measured — a tangled_rope or even snare-adjacent story, since the aristocracy had no vote in redefining the very vocabulary used to judge them. The engine's tangled_rope classification for this constraint should reflect that both a real coordination function (reduced private violence) and a real asymmetric cost (aristocratic status extraction) are present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Bourgeois professionals and clergy/reformers sit near the beneficiary end: the redefinition converts their existing dispositions (restraint, legal compliance, interior virtue) into elite currency at zero cost to themselves. The aristocratic honor culture and dueling specialists sit near the target end: they experience real loss (of status vocabulary, of occupational niche) that cannot be recovered by any individual choice, because the loss is conceptual, not merely regulatory — you cannot simply keep dueling and be seen as honorable once the vocabulary has moved. This is why exit_options for the declining aristocracy is 'constrained' rather than 'trapped' — no law prevents them from dueling, but doing so no longer accomplishes what it once did.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating honor claims without state violence) is contested as live vs. dead: the state judicial monopoly claims the problem persists and is now solved better by courts; legal historians note the underlying coordination problem persisted but was resolved by substitute institutions (libel law, professional codes) rather than the old vocabulary surviving in altered form. This prevents the naive claim that dueling's disappearance was pure victory of coordination over violence — some of what disappeared was aristocratic capacity to contest the redefinition at all, which is a mandatrophy-relevant asymmetry: the mandate (settling honor disputes) didn't vanish, it was captured by new arbiters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    redefinition_vs_cost_causal_priority,
    'Did the redefinition of honor vocabulary cause dueling''s decline independently of rising external costs (legal, social, economic), or did cost escalation drive the redefinition as post-hoc justification?',
    'Comparative historical analysis of jurisdictions/periods where honor vocabulary shifted without corresponding cost escalation, and vice versa — if dueling declined wherever vocabulary shifted regardless of enforcement intensity, contraction is corroborated; if decline tracks enforcement/cost curves regardless of vocabulary, drop is corroborated.',
    'If cost escalation is actually causally prior and vocabulary shift is rationalization, this story''s claimed_type and beneficiary structure would need revision toward the drop_reading''s structure, where extraction is more directly tied to enforcement apparatus rather than conceptual capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redefinition_vs_cost_causal_priority, conceptual, 'Whether contraction_reading''s causal claim (vocabulary shift is primary) survives against the rival drop_reading''s claim (cost escalation is primary).').

omega_variable(
    aristocratic_voice_in_redefinition,
    'Did aristocratic honor culture participate in redefining honor''s meaning (e.g., adapting duel-adjacent codes into acceptable forms like the code of the gentleman), or was the redefinition imposed entirely by rising bourgeois/clerical/state actors without aristocratic input?',
    'Textual analysis of honor manuals and courtesy literature across the interval to trace whether aristocratic authors contributed to or merely received the new vocabulary.',
    'If aristocratic actors substantially co-authored the new vocabulary, the victim/beneficiary asymmetry is weaker than authored here and the classification should move toward rope; if the redefinition was imposed unilaterally, tangled_rope (or even snare) is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aristocratic_voice_in_redefinition, empirical, 'Whether the declining aristocracy had genuine voice in the vocabulary shift that dispossessed it.').

omega_variable(
    kernel_framing_choice,
    'Is the contraction_reading the correct primary framing for this historical episode, or does the composite_reading better capture the actual causal structure (both mechanisms operating together, inseparably)?',
    'Historiographical consensus-tracking: which reading do specialist legal historians treat as the dominant explanatory frame in recent scholarship, and does that consensus shift the ε assigned to each sibling constraint?',
    'If composite_reading is judged the more accurate historical account, this story''s isolation of the conceptual mechanism alone becomes an analytically useful decomposition rather than a competing empirical claim — the two would coexist as complementary partial accounts rather than rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Framing choice between treating contraction as an independent causal story versus a component of an overdetermined composite account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1650, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_violence_legitimacy__contraction_reading, theater_ratio, 1650, 0.1).
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.36).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1650, 0.15).
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.18).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.27).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1650, 0.55).
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.37).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.1).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the honor_violence_legitimacy kernel. contraction_reading (this story) isolates conceptual redefinition of honor as the primary mechanism; drop_reading isolates cost-escalation under an unchanged concept of honor as the primary mechanism; composite_reading holds both operated inseparably. Each carries its own stable ε and beneficiary/victim structure per the ε-invariance principle — they are not the same constraint measured differently, but three structurally distinct claims about the same historical episode, linked here for contamination and cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
