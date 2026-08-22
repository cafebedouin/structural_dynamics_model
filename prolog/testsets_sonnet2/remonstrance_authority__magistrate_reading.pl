% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Right of Remonstrance (Magistrate Reading — Ancient Liberties Against Arbitrary Innovation)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the magistrate reading of the contested
 *   remonstrance-authority kernel: the Parlements' right to refuse
 *   registration of royal edicts, and to remonstrate against them, framed
 *   from the magistracy's own constitutional self-understanding as
 *   guardianship of ancient fundamental law against arbitrary royal
 *   innovation. Under this reading, the standing arrangement under contest is
 *   the remonstrance mechanism as it actually operated across the eighteenth
 *   century — repeatedly invoked against fiscal reform edicts that would have
 *   reduced noble and office-holding tax exemptions, most visibly during the
 *   vingtième and later Calonne/Brienne reform crises. ε is authored high
 *   (0.68) because, assessed by the magistrate reading's own lights applied
 *   to the arrangement's actual operation rather than its founding ideal, the
 *   mechanism's most consequential exercises consistently preserved a narrow
 *   class's fiscal privilege at the expense of an unrepresented tax base and
 *   a fiscally cornered crown. The sibling crown_reading, generated
 *   separately, authors this same standing arrangement as illegitimate
 *   minoritarian veto — a different ε is expected there and is not reconciled
 *   here per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - parlementaire_magistracy: administers remonstrance, tax-exempt beneficiary (institutional/arbitrage) — sets the agenda and collects fiscal shelter
 *   - office_holding_nobility: beneficiary (powerful/constrained) — shielded exemptions
 *   - royal_fiscal_administration: payer (institutional/constrained) — bears blocked reform costs
 *   - unrepresented_taxpaying_commoners: payer (powerless/trapped) — bears the residual tax burden with no voice
 *   - crown_reform_ministers: payer (powerful/constrained) — bears career and state-solvency risk
 *   - crown_sovereign_authority: excluded seat within this reading — its plenary-sovereignty claim is treated as the arbitrary innovation being checked
 *   - constitutional_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.58).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Right of Remonstrance (Magistrate Reading — Ancient Liberties Against Arbitrary Innovation)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '86533316-4c51-4452-bc1e-b2101c28be25').
narrative_ontology:cs_kernel_codification('86533316-4c51-4452-bc1e-b2101c28be25', distributed).
narrative_ontology:cs_authority_grounding('86533316-4c51-4452-bc1e-b2101c28be25', lineage).
narrative_ontology:cs_interpretation_layer_present('86533316-4c51-4452-bc1e-b2101c28be25').
narrative_ontology:cs_reading_relation('86533316-4c51-4452-bc1e-b2101c28be25', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('86533316-4c51-4452-bc1e-b2101c28be25', foundational, fundamental_law_binds_sovereign_will).
narrative_ontology:cs_axiom_status(fundamental_law_binds_sovereign_will, holdable).
narrative_ontology:cs_axiom_grounding('86533316-4c51-4452-bc1e-b2101c28be25', fundamental_law_binds_sovereign_will, conventional).
narrative_ontology:cs_axiom('86533316-4c51-4452-bc1e-b2101c28be25', secondary, parlement_as_lineal_inheritor_of_curia_regis_review_function).
narrative_ontology:cs_axiom_status(parlement_as_lineal_inheritor_of_curia_regis_review_function, holdable).
narrative_ontology:cs_axiom_grounding('86533316-4c51-4452-bc1e-b2101c28be25', parlement_as_lineal_inheritor_of_curia_regis_review_function, conventional).
narrative_ontology:cs_reference_frame('86533316-4c51-4452-bc1e-b2101c28be25', composite_customary_monarchy_with_conditional_registration).
narrative_ontology:cs_drift_state('86533316-4c51-4452-bc1e-b2101c28be25', pre_revolutionary_fiscal_crisis_1770s_1780s, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('86533316-4c51-4452-bc1e-b2101c28be25', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlementaire_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, office_holding_nobility).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_venal_officeholders).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_fiscal_administration).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, unrepresented_taxpaying_commoners).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_reform_ministers).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, fundamental_law_doctrine).
narrative_ontology:constraint_vindicates(remonstrance_authority__magistrate_reading, constitutional_continuity_of_ancient_liberties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Registers royal edicts as a precondition of their legal force within its jurisdiction, and exercises the right to remonstrate — to refuse registration and return the edict with objections — before the crown can compel enforcement by lit de justice. Frames this power as the guardian function of a body descended from the ancient royal council, standing between an arbitrary sovereign will and the fundamental laws of the realm. Its own members hold venal, heritable offices exempt from many of the very taxes the crown seeks to impose, so the remonstrance mechanism it administers also shields its own fiscal position.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementaire_magistracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlementaire_magistracy, beneficiary).

% Holds hereditary or purchased offices whose privileges — tax exemption chief among them — depend on the Parlements' capacity to block or delay fiscal edicts that would erode noble exemptions. Benefits from remonstrance without administering it directly; its wealth and legal standing are preserved each time an edict is remonstrated into abeyance or watered down.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, office_holding_nobility, beneficiary,
    powerful, generational, constrained, national).

% Purchased offices in provincial parlements and cours des comptes as investments carrying fiscal and status privileges. Depends on the remonstrance mechanism to protect the value of those offices against royal edicts that would abolish venality, extend taxation to office-holders, or centralize fiscal administration.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_venal_officeholders, beneficiary,
    organized, biographical, constrained, regional).

% Drafts fiscal reform edicts — new taxes, restructured collection, reduced exemptions — needed to address structural deficits and war financing. Each edict can be blocked, delayed, or forced into repeated remonstrance cycles by the Parlements, which the administration must overcome by holding a lit de justice or by making concessions that hollow out the reform. Its capacity to govern fiscal policy is bounded by whichever Parlement controls registration for the affected jurisdiction.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_fiscal_administration, payer,
    institutional, immediate, constrained, national).

% Bears the tax burden that remains after noble and office-holding exemptions are preserved through successful remonstrance. Has no seat in the remonstrance process, no standing to petition the Parlement directly on tax equity, and no exit from the jurisdiction's fiscal obligations. When a reform edict that would broaden the tax base toward the privileged orders is remonstrated away, the shortfall is made up disproportionately through indirect taxes and the taille.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, unrepresented_taxpaying_commoners, payer,
    powerless, biographical, trapped, national).

% Ministers who stake their careers and the fiscal solvency of the state on reforms that remonstrance can indefinitely stall. Repeated remonstrance cycles force ministers to choose between confrontation (forced registration, exile of magistrates) and abandonment of reform, and the political cost of confrontation frequently ends ministerial careers.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_reform_ministers, payer,
    powerful, biographical, constrained, national).

% Holds, in its own doctrine, plenary and undivided legislative sovereignty not subject to conditional ratification by any subordinate body. From this seat the remonstrance mechanism is a usurpation dressed as guardianship; but within the magistrate reading this seat's claim is treated as the arbitrary innovation the mechanism exists to check, not as a legitimate party to be accommodated.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_sovereign_authority, excluded,
    institutional, civilizational, trapped, national).

% Study the documentary record of remonstrance episodes, the Parlements' own self-justifying jurisprudence, and the fiscal outcomes of blocked reforms to assess whether the mechanism functioned as a constitutional check or as an entrenchment device for a narrow propertied and office-holding class.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlementaire_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, non-violent channel through which an institutional body with deep knowledge of prior law and precedent can flag that a proposed royal edict conflicts with established fundamental law, forcing deliberation and revision before the edict takes binding force — a genuine check against purely unilateral legislative innovation.
% TRANSFER_FUNCTION: Moves fiscal exposure from the office-holding and noble classes, whose exemptions the remonstrance mechanism repeatedly preserves, onto the unrepresented taxpaying population and onto the crown's capacity to finance its obligations; moves political risk from the magistracy (which bears little cost for remonstrating) onto reform ministers (who bear career and state-solvency costs for pressing edicts through).
% ABSENT_VOICES: Unrepresented taxpaying commoners have no standing before the Parlement and no voice in what counts as a fundamental law worth defending; provincial estates and non-officeholding bourgeoisie are likewise absent from the remonstrance conversation despite bearing its fiscal consequences.
% DISAPPEARANCE_RATIONALE: If remonstrance disappeared overnight, royal fiscal edicts would take immediate legal force upon promulgation; office-holding tax exemptions would lose their principal institutional shield; ministers could pursue reform without the multi-year attrition of registration battles; and the Parlements would lose the leverage that constitutes their political relevance beyond judicial administration.
% FOUNDING_PROBLEM: Originally arose from the Parlement's function as registrar of royal acts within its jurisdiction — a genuine administrative and juridical review step meant to catch edicts inconsistent with prior law, drafting errors, or conflicts with established custom before they entered force.
% FOUNDING_PROBLEM_CORROBORATION: The Parlements themselves attest the founding problem is fully live — that fundamental law requires an institutional guardian against arbitrary royal will. Crown ministers, and later revolutionary-era pamphleteers writing from outside the magistracy's own ranks, attest the registration-and-review function had long since been captured by a self-interested venal office-holding class defending fiscal privilege under the language of ancient liberty; no source independent of one side or the other offers an unconflicted account.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.42 to 0.68) tracking the escalating fiscal crisis: as the crown's need for revenue intensified from the 1750s onward, the remonstrance mechanism was invoked with increasing frequency specifically against edicts targeting noble and office-holder exemptions, making the extractive function more visible and more central to the mechanism's actual use even as its constitutional rhetoric remained constant. Suppression (0.58 by 1789) reflects the escalating machinery needed to sustain the arrangement — repeated lits de justice, exiles of magistrates, and cycles of confrontation and restoration — required specifically because the mechanism faced increasing resistance from a fiscally desperate crown. Theater ratio is kept comparatively low (0.28) because the review-and-registration function was not empty performance; it retained real juridical content even as its exemption-preserving function grew more dominant. Accessibility collapse is authored moderate (0.42), not high, because alternative fiscal paths (forced registration, provincial estates, eventual convocation of the Estates-General) remained genuinely available throughout — this was not a closed mountain-grade constraint. Resistance is authored high (0.71) reflecting the crown's persistent, escalating attempts to overcome or bypass the mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The parlementaire_magistracy sits at the clear beneficiary pole: it administers the mechanism, collects the tax-shelter benefit of its own venal offices, and faces essentially no personal fiscal exposure for exercising remonstrance (d near beneficiary). Office-holding nobility and provincial venal officeholders benefit structurally without administering the mechanism directly. Royal fiscal administration and crown reform ministers sit near the target pole — constrained exit (they cannot simply route around Parlement jurisdiction without extraordinary measures), bearing the transfer cost directly through blocked or diluted reforms. Unrepresented taxpaying commoners sit at the extreme target pole: trapped exit, powerless, and bearing the downstream fiscal cost of every successful remonstrance without ever appearing as a party to the remonstrance conversation itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The magistrate reading's own doctrine treats the founding problem — checking arbitrary royal legislative innovation against fundamental law — as still fully live in 1789, which would argue against mandatrophy. But the founding_problem_status is authored 'contested' rather than 'live' outright, because the corroboration record shows the mechanism's actual deployment increasingly tracked fiscal self-interest of the office-holding class rather than a neutral constitutional-review function; the tangled_rope classification (rather than a claimed pure rope or mountain) is precisely the vehicle for holding the genuine coordination function and the asymmetric extraction together without collapsing either into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_law_doctrine_authenticity,
    'Is the Parlements'' invocation of ''fundamental law'' and ''ancient liberties'' a genuine constitutional constraint inherited from an actual historical constitutional order, or a retrospectively constructed doctrine deployed to legitimate resistance to specific fiscal edicts?',
    'Comparative textual analysis of remonstrance documents across the seventeenth and eighteenth centuries to see whether the invoked ''fundamental laws'' track a stable, pre-existing body of law or shift opportunistically to match whichever edict is currently being resisted.',
    'If the doctrine is shown to be substantially retrospective and edict-specific, the mountain-adjacent framing (ancient, natural, unchangeable liberty) collapses further toward tangled_rope or snare; if a stable pre-existing body of fundamental law is demonstrated, the magistrate reading''s constitutional claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_law_doctrine_authenticity, conceptual, 'Whether the invoked ancient liberties are a genuine inherited constraint or a constructed legitimating narrative.').

omega_variable(
    committer_framing_kernel_disagreement,
    'This story is one reading (magistrate_reading) of the remonstrance_authority kernel; the sibling crown_reading treats the identical standing arrangement as an illegitimate minoritarian veto. Where exactly does the disagreement between readings locate itself structurally?',
    'The disagreement is located in the authority_grounding of the kernel itself: the magistrate reading grounds legitimacy in lineage/practice (the Parlement as inheritor of the ancient royal council''s advisory-and-review function); the crown reading grounds legitimacy in the doctrine of undivided royal sovereignty, under which no subordinate body can condition the sovereign''s legislative will. Resolving which grounding is constitutionally correct is not an empirical question resolvable by further historical data — it is a question of which theory of sovereignty (composite/customary vs. unitary/absolutist) is taken as fundamental, which the two readings answer oppositely by design.',
    'Under the magistrate reading, crown_sovereign_authority''s plenary claim is the arbitrary innovation being checked (this story''s framing). Under the crown reading, the Parlements'' remonstrance is itself the illegitimate innovation departing from proper sovereign authority. The two readings cannot be merged; each computes its own classification from its own authority_grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_kernel_disagreement, conceptual, 'Location of the magistrate/crown reading disagreement in competing theories of where legislative sovereignty properly resides.').

omega_variable(
    counterfactual_reform_outcome,
    'Had remonstrance not existed or been routinely overridden without cost, would fiscal reforms extending taxation to the privileged orders have succeeded in averting the fiscal collapse that preceded 1789, or would other structural obstacles (venality, provincial estates, tax-farming) have produced similar outcomes regardless?',
    'Comparative analysis against monarchies without an equivalent remonstrance mechanism (e.g., contemporary Prussian or Habsburg fiscal reform attempts) to isolate remonstrance''s marginal causal contribution to reform failure from other structural barriers.',
    'If remonstrance is shown to be the decisive obstacle, the case for high ε and victim status for royal_fiscal_administration and unrepresented_taxpaying_commoners strengthens considerably; if comparable fiscal reform failures occurred in systems without remonstrance, the mechanism''s causal weight in this story should be discounted relative to structural fiscal-administrative weaknesses common across ancien-regime states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reform_outcome, empirical, 'Whether remonstrance was a decisive versus incidental cause of failed fiscal reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1715, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1715, remonstrance_authority__magistrate_reading, theater_ratio, 1715, 0.15).
narrative_ontology:measurement(remo_tr_t1730, remonstrance_authority__magistrate_reading, theater_ratio, 1730, 0.18).
narrative_ontology:measurement(remo_tr_t1748, remonstrance_authority__magistrate_reading, theater_ratio, 1748, 0.21).
narrative_ontology:measurement(remo_tr_t1763, remonstrance_authority__magistrate_reading, theater_ratio, 1763, 0.24).
narrative_ontology:measurement(remo_tr_t1776, remonstrance_authority__magistrate_reading, theater_ratio, 1776, 0.26).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.28).

% Extraction over time
narrative_ontology:measurement(remo_be_t1715, remonstrance_authority__magistrate_reading, base_extractiveness, 1715, 0.42).
narrative_ontology:measurement(remo_be_t1730, remonstrance_authority__magistrate_reading, base_extractiveness, 1730, 0.48).
narrative_ontology:measurement(remo_be_t1748, remonstrance_authority__magistrate_reading, base_extractiveness, 1748, 0.55).
narrative_ontology:measurement(remo_be_t1763, remonstrance_authority__magistrate_reading, base_extractiveness, 1763, 0.61).
narrative_ontology:measurement(remo_be_t1776, remonstrance_authority__magistrate_reading, base_extractiveness, 1776, 0.65).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1715, remonstrance_authority__magistrate_reading, suppression_requirement, 1715, 0.35).
narrative_ontology:measurement(remo_su_t1730, remonstrance_authority__magistrate_reading, suppression_requirement, 1730, 0.4).
narrative_ontology:measurement(remo_su_t1748, remonstrance_authority__magistrate_reading, suppression_requirement, 1748, 0.47).
narrative_ontology:measurement(remo_su_t1763, remonstrance_authority__magistrate_reading, suppression_requirement, 1763, 0.5).
narrative_ontology:measurement(remo_su_t1776, remonstrance_authority__magistrate_reading, suppression_requirement, 1776, 0.55).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, venal_office_tax_exemption).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, lit_de_justice_forced_registration).

% DUAL FORMULATION NOTE:
% This constraint and remonstrance_authority__crown_reading are the two declared readings of the single remonstrance_authority kernel. Both describe the same standing historical arrangement — the Parlements' registration-and-remonstrance practice — but author different ε, different beneficiary/victim structure, and different claimed_type from their respective authority_grounding premises (lineage/practice for the magistrate reading; unitary sovereignty for the crown reading). They are linked here rather than merged, per the ε-invariance decomposition principle; each carries its own stable ε rather than an averaged or hedged value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
