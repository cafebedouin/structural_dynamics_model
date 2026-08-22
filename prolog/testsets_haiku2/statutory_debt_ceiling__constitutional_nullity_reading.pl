% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void Under 14th Amendment Section 4
 *   domain: constitutional_law/political_economy
 *
 * SUMMARY:
 *   The statutory debt ceiling is a recurring legislative brake on Treasury
 *   borrowing, nominally capped at a fixed amount and requiring periodic
 *   Congressional reauthorization. The constitutional nullity reading argues
 *   that the 14th Amendment Section 4's mandate to preserve the public credit
 *   renders the ceiling legally void: Treasury's constitutional obligation to
 *   service lawful appropriations cannot be suspended by a statute that
 *   contradicts that obligation. Under this reading, the constraint has zero
 *   extractiveness because it binds nothing — the ceiling is theater, not
 *   law. Congressional votes on ceiling increases are ceremonial; Treasury
 *   executes borrowing as required by appropriations regardless of ceiling
 *   levels. The reading forecloses the extraction-via-default-threat
 *   narrative and differs structurally from coordination and snare readings
 *   that treat the ceiling as operative.
 *
 * KEY AGENTS:
 *   - Treasury Department: institutional actor executing appropriations; under this reading, ceiling-free
 *   - Congress (legislative branch): dual-positioned—retains control via appropriations but loses secondary leverage via ceiling brinkmanship
 *   - Judicial Branch: observer seat; would certify the constitutional void through litigation or advisory opinion
 *   - Bond Markets: beneficiary of certainty; stable Treasury finance under constitutional guarantee
 *   - Political Minorities: excluded from ceiling-weaponization plays by constitutional nullity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.92).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.92).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void Under 14th Amendment Section 4").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'e41a5748-83a2-4ae6-8c71-d024a88244b5').
narrative_ontology:cs_kernel_codification('e41a5748-83a2-4ae6-8c71-d024a88244b5', fixed_text).
narrative_ontology:cs_authority_grounding('e41a5748-83a2-4ae6-8c71-d024a88244b5', lineage).
narrative_ontology:cs_interpretation_layer_present('e41a5748-83a2-4ae6-8c71-d024a88244b5').
narrative_ontology:cs_reading_relation('e41a5748-83a2-4ae6-8c71-d024a88244b5', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('e41a5748-83a2-4ae6-8c71-d024a88244b5', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('e41a5748-83a2-4ae6-8c71-d024a88244b5', foundational, section_four_self_executing_supremacy).
narrative_ontology:cs_axiom_status(section_four_self_executing_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e41a5748-83a2-4ae6-8c71-d024a88244b5', section_four_self_executing_supremacy, deontological).
narrative_ontology:cs_axiom('e41a5748-83a2-4ae6-8c71-d024a88244b5', foundational, public_credit_mandate_undefeatable).
narrative_ontology:cs_axiom_status(public_credit_mandate_undefeatable, holdable).
narrative_ontology:cs_axiom_grounding('e41a5748-83a2-4ae6-8c71-d024a88244b5', public_credit_mandate_undefeatable, deontological).
narrative_ontology:cs_reference_frame('e41a5748-83a2-4ae6-8c71-d024a88244b5', constitutional_public_credit_supremacy).
narrative_ontology:cs_drift_state('e41a5748-83a2-4ae6-8c71-d024a88244b5', contemporary_debt_ceiling_disputes, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e41a5748-83a2-4ae6-8c71-d024a88244b5', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_legitimacy_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress_legislative_branch).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bond_markets_investors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congress_legislative_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues debt instruments as directed by appropriations law. Under this reading, the debt ceiling is a legal nullity and the Treasury operates subject only to appropriations mandates. The constraint on Treasury's actual borrowing authority is zero; the ceiling's persistence is theatrical, not operative. Treasury's position: the ceiling contradicts the 14th Amendment's mandate to preserve the public credit and cannot limit borrowing required to execute lawful appropriations.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, generational, analytical, national).

% Under this reading, Congress retains power to appropriate but cannot use a debt ceiling to block Treasury execution of those appropriations. The ceiling votes become ceremonial — Congress appropriates, Treasury borrows what appropriations require, ceiling debates produce no binding constraint. Congress is thus 'beneficiary' (retains control via appropriations) and 'payer' (loses its secondary power to weaponize the ceiling as a blocking tool).
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_legislative_branch, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress_legislative_branch, payer).

% Interprets the 14th Amendment Section 4 and the interaction between that clause and the statutory debt ceiling. Under this reading, the courts would uphold Treasury's borrowing as constitutional enforcement of the 14th Amendment's public credit mandate, rendering the ceiling legally void. Judicial action would move the constraint from theater to settled constitutional law.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, judicial_branch, observer,
    institutional, generational, analytical, national).

% Under this reading, the Treasury's borrowing is constitutionally guaranteed to execute, eliminating the risk premium associated with ceiling-induced default threat. Bond markets price in the nullity of the ceiling and price out the political volatility it otherwise introduces. Investors benefit from stable, predictable Treasury finance.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bond_markets_investors, beneficiary,
    organized, biographical, arbitrage, global).

% Under this reading, a legislative minority loses the power to extract concessions by threatening default via ceiling brinkmanship. The constitutional nullity reading forecloses the ceiling as a leverage point for minority extraction. Political minorities that previously wielded the ceiling as a veto tool are structurally excluded from that play.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, political_minority_factions, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute's nominal function is procedural: to require Congress to periodically reaffirm borrowing authority rather than authorizing unlimited borrowing in a single appropriations act. Under this reading, that coordination function is void — the 14th Amendment directly mandates Treasury execution regardless of the statute.
% TRANSFER_FUNCTION: Under this reading, no transfer occurs via the constraint itself. The constraint is legally inoperative and produces no extraction. The statute's text purports to authorize borrowing up to a cap, but that cap is constitutionally void and does not bind Treasury action.
% ABSENT_VOICES: Legislative minorities that use the ceiling as a negotiating weapon (they do not enter the reading's logic — the reading forecloses their use of the constraint as a tool). Citizens who depend on government services funded by borrowing and stand to benefit from stable Treasury finance (they are not at the negotiating table when ceiling brinkmanship occurs). Economists who argue the ceiling serves a fiscal-prudence function (their voice is excluded from the constitutional reading, which treats the ceiling as legally void regardless of fiscal policy merits).
% DISAPPEARANCE_RATIONALE: Under this reading, the ceiling is already void; its 'disappearance' has no effect because it binds nothing. The constraint is theater that persists despite its legal nullity. If the ceiling statute were formally repealed, nothing would change in Treasury's actual borrowing behavior or authority — it was already unconstrained by it.
% FOUNDING_PROBLEM: Post-Civil War, Congress needed to confirm the nation's obligation to service the war debt and affirm the public credit. The 14th Amendment Section 4 ('the validity of the public debt of the United States shall not be questioned') was the constitutional solution: it placed public credit obligation above legislative revision or default threat.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars (Paulsen, Buchanan, Dorf) who have argued the 14th Amendment Section 4 directly nullifies the debt ceiling. The constraint's founding problem (securing the public debt against legislative default) remains live because periodic default threats have recurred. The nullity reading is corroborated by legal historical analysis, though not yet by binding judicial precedent.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the constraint is legally void—it produces no binding transfer or extraction. The ceiling does not extract from Treasury or constrain appropriations execution; it is unenforceable against the 14th Amendment. Suppression is zero because there is no coercive mechanism—the constraint is inert law. Theater ratio is high (0.92) because the constraint's persistence is entirely performative: Congress holds ceiling votes, media reports deadlines, political actors negotiate as if the ceiling matters, yet Treasury's actual borrowing authority and obligation remain unchanged. The constraint's mechanism (a legislated dollar cap) produces no effect; all observable activity around it is theater. Accessibility of alternatives does not collapse (0.98 is high because under this reading the constraint is so legally void that alternatives—unconstrained borrowing—are immediate and available; the only barrier is political theater, not legal constraint). Resistance is low (0.15) because the constraint meets minimal opposition on a legal-nullity reading—courts would uphold Treasury; the 14th Amendment's text is clear on its face; the only resistance comes from political actors who exploit the theater for leverage.
 *
 * PERSPECTIVAL GAP:
 *   The Treasury seat and the judicial seat compute convergently (the constraint binds neither; both see it as void). Congress and political minorities compute divergently: Congress may experience the constraint as real (the ceiling's procedural requirement to reauthorize borrowing creates repeated legislative moments where opponents can extract concessions), but the nullity reading denies that the ceiling has binding force—it is political theater that Congress participates in despite the absence of legal constraint. Political minorities experience the ceiling as a live leverage point in the nullity reading's world, but only as theater, not as a legal ceiling. The Engine would compute low or zero d for all seats under the nullity reading because none of them are structurally targeted by a void constraint; all sit at the beneficiary end (they all benefit from certainty, or are excluded entirely from a mechanism that does not work).
 *
 * DIRECTIONALITY LOGIC:
 *   Under the nullity reading, directionality is not meaningfully differentiated because the constraint is void—there are no targets. Treasury is not targeted by a void ceiling. Congress is not targeted because it retains full control via appropriations. All stakeholders sit at or near the beneficiary end because the constraint either does not bind (Treasury, bond markets) or has been foreclosed as a leverage point (political minorities cannot use a void constraint). The scenario does not generate the structural asymmetry (beneficiary/victim split) typical of extractive constraints; the nullity reading flattens directionality because it denies the constraint's operative status.
 *
 * MANDATROPHY ANALYSIS:
 *   The nullity reading presents a mandatrophy candidate: the founding problem (securing the public credit against legislative default threat) remains live, but the constraint's relationship to that problem has inverted. The ceiling was founded to prevent default by requiring periodic reaffirmation of debt service. Under the nullity reading, the ceiling is legally incapable of preventing default because it contradicts the 14th Amendment's mandate. The constraint persists in theater form (Congress votes, media reports, political actors negotiate) but has been displaced from its founding mandate. The nullity reading asserts mandatrophy is resolved—the constraint is legally void and therefore does not constitute a failed mandate, but rather a ceremonial remnant. The coordination and snare readings would deny the mandatrophy: they treat the ceiling as operative and argue about its function (coordination vs. extraction). The nullity reading forecloses the mandatrophy debate by asserting the ceiling is legally null.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_nullity_vs_political_persistence,
    'If the debt ceiling is legally void under the 14th Amendment, how does it persist as a political constraint that generates recurring default crises?',
    'Judicial ruling on 14th Amendment Section 4 supremacy over the statutory ceiling. Constitutional scholars'' analysis of the amendment''s self-executing force vs. legislative implementation. Comparative study of jurisdictions where constitutional provisions explicitly nullify conflicting statutes (e.g., other constitutions with parallel public credit clauses).',
    'If courts rule the ceiling is void and unenforceable, the political theater dissipates and the constraint transitions from piton (theater + legal nullity) to definitively foreclosed. If courts decline to rule or rule the ceiling remains operative despite 14th Amendment tension, the nullity reading is not judicially endorsed and the constraint reverts to contested legal status. This resolves which of the three readings (nullity, scaffold, snare) is legally binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_nullity_vs_political_persistence, empirical, 'Whether the 14th Amendment Section 4 is self-executing in voiding the debt ceiling or requires Congressional implementation to have force.').

omega_variable(
    amending_power_conflict,
    'Can Congress use an ordinary statute (the ceiling) to override or suspend a constitutional mandate (the 14th Amendment)?',
    'Constitutional law doctrine on supremacy of constitutional mandates over ordinary legislation. Historical analysis of prior instances where ordinary statutes attempted to constrain constitutional mandates (e.g., habeas corpus suspension, war powers). Originalist vs. living-constitution interpretation of Section 4''s scope and self-executing character.',
    'If the Constitution''s mandate is supreme and self-executing, the ceiling is void and the nullity reading is correct. If Congress retains power to condition or delay borrowing even for constitutionally-required debt service, the ceiling is operative and the nullity reading is incorrect. This divides constitutional vs. political economy readings of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amending_power_conflict, conceptual, 'Whether the constitutional hierarchy prevents a statute from overriding the 14th Amendment''s public credit mandate.').

omega_variable(
    false_summit_beneficiary_ambiguity,
    'Is the ''constitutional legitimacy framework'' identified as a beneficiary a natural outcome of the 14th Amendment, or a constructed reading that benefits parties arguing for unlimited Treasury borrowing?',
    'Originalist textual analysis of the 14th Amendment''s framers'' intent regarding public credit and statutory borrowing limits. Examination of whether 19th-century Congress believed Section 4 foreclosed debt ceilings. Identification of who actually profits from a nullity ruling (organized labor, entitlements advocates, the institutional executive branch) vs. who opposes it (fiscal conservatives, creditor interests).',
    'If the nullity reading is a genuine constitutional meaning, the beneficiary is the constitutional framework itself (a natural outcome). If the nullity reading is a post-hoc construction, the identified beneficiary (Treasury, broad constituencies dependent on government spending) reveals a false-summit dynamic where a constructed constraint benefits specific actors while claiming constitutional naturalness. This shifts classification toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_ambiguity, conceptual, 'Whether the nullity reading emerges naturally from constitutional text or is a constructed reading that benefits identifiable parties.').

omega_variable(
    theater_ratio_meaning_under_nullity,
    'If the constraint is legally void, what accounts for the rising theater_ratio from 0.05 (1913) to 0.92 (2026)? Is the rising theater a symptom of degrading legal authority, or a separate dynamic?',
    'Historical analysis of when debt-ceiling brinkmanship and default threats became visible political tools (circa 1995 first shutdown; escalation post-2008). Identification of whether rising theater corresponds to periods when the ceiling was actively used as leverage vs. periods of routine reauthorization. Counterfactual: if Congress had not voted on the ceiling and Treasury simply borrowed as appropriations required, would the political dynamics have changed?',
    'If rising theater tracks escalating political weaponization of a constraint Congress knows (or should know) is legally void, the constraint is a piton: inert law maintained in theaters as a leveraging tool by actors who understand its legal status but exploit its political salience. If rising theater tracks increasing doubt about the ceiling''s legal authority, the trajectory shows movement from mountain (unchallenged legal status) toward piton (theatrical remnant). Either way, the nullity reading''s claim that the constraint is void is consistent with the measurement data—the theater rise documents the gap between legal status and political persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_meaning_under_nullity, empirical, 'What the rising theater_ratio trajectory reveals about the constraint''s transition from (possibly) operative to definitely ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1913, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1913, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1913, 0.05).
narrative_ontology:measurement(stat_tr_t1950, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(stat_tr_t1980, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(stat_tr_t2000, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2000, 0.72).
narrative_ontology:measurement(stat_tr_t2015, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2015, 0.88).
narrative_ontology:measurement(stat_tr_t2026, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2026, 0.92).

% Extraction over time
narrative_ontology:measurement(stat_be_t1913, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1913, 0.0).
narrative_ontology:measurement(stat_be_t1950, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement(stat_be_t1980, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1980, 0.0).
narrative_ontology:measurement(stat_be_t2000, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(stat_be_t2015, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2015, 0.0).
narrative_ontology:measurement(stat_be_t2026, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2026, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The statutory_debt_ceiling kernel decomposes into three readings with distinct ε values and structural positions: (1) constitutional_nullity_reading (this constraint): ε=0.0, legally void, mountain-framed as natural constitutional law; (2) coordination_scaffold_reading: ε~0.15-0.25, procedural mechanism enabling Treasury operations without repeated micromanagement, rope-framed; (3) extraction_snare_reading: ε~0.65-0.85, weaponized boundary enabling minority extraction via default threat, snare-framed. The three readings are held simultaneously by different parties: constitutional scholars and Treasury administrators endorse nullity; legislative leadership treats ceiling votes as routine coordination; political minorities exploit ceiling brinkmanship as leverage. The nullity reading's axioms directly foreclose the snare reading's core premise (that the ceiling is a usable veto point) and constrain the scaffold reading's nominal function (periodic reauthorization is ceremonial, not genuinely coordinating). All three readings share the same legal text (the 1917 statute and the 14th Amendment) and the same historical interval but instantiate different constraints from the same kernel via reading-dependent interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
