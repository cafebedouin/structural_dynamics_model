% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Remonstrance Right as Fundamental Constitutional Mechanism (Magistrate Reading)
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the magistrate reading of the remonstrance
 *   authority kernel in Old Regime France. Under this reading, the right of
 *   remonstrance exercised by the sovereign courts (parlements) is a
 *   fundamental constitutional mechanism that preserves ancient liberties
 *   against arbitrary royal innovation, particularly in taxation. The
 *   constraint creates a genuine coordination functionâjudicial review of
 *   edicts, protection of fundamental lawâwhile simultaneously generating a
 *   beneficiary class of tax-exempt hereditary magistrates and extracting
 *   from the crown's fiscal reform capacity. When the crown overrides
 *   remonstrances through lits de justice or political suppression, the
 *   parlementary benches themselves enter the victim set. The sibling
 *   crown_reading treats the same practice as an illegitimate minoritarian
 *   veto protecting particularist privilege.
 *
 * KEY AGENTS:
 *   - parlementary_benches: Primary agenda-setter (institutional/constrained) â exercise remonstrance and bear suppression costs when overridden
 *   - hereditary_magistracy: Primary beneficiary (powerful/constrained) â collects tax exemptions and office immunities
 *   - crown_ministry: Primary target of successful remonstrance (institutional/constrained) â bears cost of blocked fiscal reform
 *   - common_taxpayers: Excluded party (powerless/trapped) â pay inequitable taxes while fiscal reform is obstructed
 *   - constitutional_historians: Analytical observer (analytical) â evaluate the contested constitutional genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.72).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right as Fundamental Constitutional Mechanism (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '9beb8867-a708-4a7f-85a6-88374286dd83').
narrative_ontology:cs_kernel_codification('9beb8867-a708-4a7f-85a6-88374286dd83', formalized).
narrative_ontology:cs_authority_grounding('9beb8867-a708-4a7f-85a6-88374286dd83', lineage).
narrative_ontology:cs_interpretation_layer_present('9beb8867-a708-4a7f-85a6-88374286dd83').
narrative_ontology:cs_reading_relation('9beb8867-a708-4a7f-85a6-88374286dd83', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('9beb8867-a708-4a7f-85a6-88374286dd83', foundational, remonstrance_as_fundamental_law).
narrative_ontology:cs_axiom_status(remonstrance_as_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('9beb8867-a708-4a7f-85a6-88374286dd83', remonstrance_as_fundamental_law, conventional).
narrative_ontology:cs_axiom('9beb8867-a708-4a7f-85a6-88374286dd83', secondary, magisterial_immunity_as_constitutional_necessity).
narrative_ontology:cs_axiom_status(magisterial_immunity_as_constitutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9beb8867-a708-4a7f-85a6-88374286dd83', magisterial_immunity_as_constitutional_necessity, conventional).
narrative_ontology:cs_reference_frame('9beb8867-a708-4a7f-85a6-88374286dd83', ancient_constitutional_order).
narrative_ontology:cs_drift_state('9beb8867-a708-4a7f-85a6-88374286dd83', late_ancien_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9beb8867-a708-4a7f-85a6-88374286dd83', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, hereditary_magistracy).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, parlementary_benches).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_ministry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign courts (parlements) issue remonstrances against royal edicts they judge contrary to the fundamental laws of the realm, particularly arbitrary fiscal innovations. When the crown responds with lits de justice, exile of magistrates, or suspension of sessions, these institutions bear the direct suppression costs of the constraint they enforce.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlementary_benches, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlementary_benches, payer).

% The corps of parlementary judges holds venal, hereditary offices that confer substantial tax exemptions (paulette, franc-fief immunities) and high social status. The remonstrance system legitimizes their constitutional role and protects the fiscal immunities attached to their offices, regardless of whether individual edicts are overridden.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, hereditary_magistracy, beneficiary,
    powerful, biographical, constrained, national).

% The royal administration and council issue fiscal reform edicts (tailles, capitations, vingtiÃ¨mes) to meet wartime debt and state expenditure. Remonstrances block, delay, or dilute these reforms, extracting fiscal capacity and forcing reliance on borrowing, expedients, or politically safer but economically inferior revenue sources.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_ministry, payer,
    institutional, biographical, constrained, national).

% Non-privileged subjects carry the tax burden under an inequitable fiscal system that remonstrance helps preserve. They have no institutional voice in the constitutional dialogue between crown and parlements, and would benefit from fiscal reform if it reached them.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, common_taxpayers, excluded,
    powerless, biographical, trapped, national).

% Analyze the contested genealogy of the remonstrance right, the empirical distribution of remonstrance motives between public liberty and corporate privilege, and the divergence between the magistrate and crown readings of the same constitutional practice.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, constitutional_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, hereditary_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a judicial check on royal legislation and taxation, channeling resistance to arbitrary innovation through formal remonstrance rather than revolt, and preserving the realm's fundamental laws across reigns.
% TRANSFER_FUNCTION: Transfers fiscal and legislative initiative away from the crown when remonstrances block or delay edicts; transfers tax burden away from the privileged magisterial class and onto common taxpayers by protecting exemptions.
% ABSENT_VOICES: Common taxpayers and provincial non-parlementary elites who would favor fiscal equity and representative consent are structurally excluded from the remonstrance dialogue; they bear the costs of obstructed reform without a seat at the constitutional table.
% DISAPPEARANCE_RATIONALE: If the remonstrance mechanism vanished overnight, the crown could impose fiscal reform by decree, the magistracy would lose its constitutional leverage and likely its immunities, and the ancient-liberties framework would collapseârequiring either absolutist centralization or new representative institutions to channel opposition.
% FOUNDING_PROBLEM: The medieval and early modern crown's recurrent tendency to impose arbitrary taxation and legislative innovation without regard for the fundamental laws of the realm, threatening both liberty and property.
% FOUNDING_PROBLEM_CORROBORATION: The crown ministry and royal accountants attest from outside the beneficiary set that arbitrary innovation has been superseded by fiscal necessity. Constitutional historians provide mixed external corroboration, noting that while arbitrary taxation was a genuine medieval threat, the eighteenth-century parlements routinely conflated corporate self-interest with the defense of public liberty.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because remonstrance systematically blocks fiscal reform edicts, forcing the crown into inferior fiscal expedients and protecting magisterial privilege. Suppression (0.68) is high because the constraint's persistence depends on active institutional conflict: the parlements must issue remonstrances and the crown must actively override them through lits de justice and exile. Theater_ratio (0.48) is moderate-to-high because while the constitutional check has genuine historical roots, an increasing share of remonstrance activity in the eighteenth century served corporate self-defense and privilege protection rather than the public liberty function claimed. Accessibility_collapse (0.45) is moderate because alternatives (representative assemblies, absolutist centralization) existed in the political imagination but were institutionally inaccessible. Resistance (0.75) is high because the crown resisted the constraint vigorously and the parlements resisted suppression. The metrics and claim are independently authored: the magistrate reading is claimed as tangled_rope because the structure coordinates constitutional limitation while extracting fiscal capacity and privilege.
 *
 * PERSPECTIVAL GAP:
 *   From the parlementary seat, remonstrance is ancient constitutional duty; from the crown ministry seat, it is obstructionist extraction that paralyzes necessary reform; from the hereditary magistracy seat, it is the guarantee of their immunities. The engine computes these divergences from the structural data: the same institutional complex appears as coordination to the magistracy, extraction to the crown, and mixed coordination-victimization to the parlementary benches.
 *
 * DIRECTIONALITY LOGIC:
 *   Hereditary_magistracy is declared in beneficiaries and has constrained exit (venal offices bind them but confer privilege), deriving low directionality. Crown_ministry is declared in victims with constrained exit (fiscal emergency binds them), deriving high directionality. Parlementary_benches are declared in victims despite their agenda-setting role because the override moments extract suppression costs from them; their constrained exit (corporate solidarity, professional identity) also derives high directionality. The divergence between beneficiary and victim seats drives the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâarbitrary royal innovation violating fundamental lawâwas contested by the eighteenth century. The crown argued fiscal necessity, not arbitrariness, motivated reform. The parlements' persistence in remonstrance beyond the immediate threat of arbitrary taxation suggests partial mandatrophy: the mechanism outlived its founding coordination problem and became a vehicle for corporate privilege defense. The contested founding_problem_status prevents pure coordination (rope) classification, while the genuine historical constitutional function prevents pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_ambiguity,
    'Does the remonstrance right constitute a fundamental constitutional liberty preserving the realm against arbitrary innovation, or an illegitimate particularist veto protecting magisterial privilege?',
    'Archival analysis of remonstrance registers to determine whether objections cluster on constitutional principle or fiscal privilege; comparative constitutional history of judicial review origins.',
    'If the crown reading is structurally correct, the beneficiary and victim sets invert and classification shifts toward snare; if the magistrate reading holds, tangled_rope is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_ambiguity, conceptual, 'Core framing ambiguity between magistrate and crown readings of remonstrance').

omega_variable(
    ancient_liberty_historicity,
    'Is the remonstrance right a genuinely ancient constitutional limitation, or a post-hoc juridical construction elaborated by parlementary jurists in the sixteenth and seventeenth centuries?',
    'Philological and archival genealogy of the ''fundamental laws'' doctrine; examination of medieval parlementary practice versus early modern constitutional theorization.',
    'If constructed rather than ancient, the coordination claim is partly mythical, increasing theater_ratio and shifting the constraint toward a snare of privilege protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_liberty_historicity, empirical, 'Historical authenticity of the ancient liberties claim').

omega_variable(
    fiscal_reform_blockage_motive,
    'Do parlementary remonstrances against fiscal reform edicts primarily defend constitutional principle against arbitrary taxation, or protect the magisterial class''s tax exemptions and venal privileges?',
    'Content analysis of remonstrance texts; correlation between remonstrance issuance and edicts threatening privileged immunities versus those reforming general taxation.',
    'If privilege protection dominates, extraction overwhelms coordination and the constraint approaches snare; if constitutional principle dominates, the tangled_rope balance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_reform_blockage_motive, empirical, 'Remonstrance motive ambiguity between principle and privilege').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__magistrate_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__magistrate_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(remo_tr_t60, remonstrance_authority__magistrate_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(remo_tr_t80, remonstrance_authority__magistrate_reading, theater_ratio, 80, 0.43).
narrative_ontology:measurement(remo_tr_t100, remonstrance_authority__magistrate_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__magistrate_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__magistrate_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(remo_be_t60, remonstrance_authority__magistrate_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(remo_be_t80, remonstrance_authority__magistrate_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(remo_be_t100, remonstrance_authority__magistrate_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__magistrate_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__magistrate_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(remo_su_t60, remonstrance_authority__magistrate_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(remo_su_t80, remonstrance_authority__magistrate_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(remo_su_t100, remonstrance_authority__magistrate_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
