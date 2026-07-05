% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist-Paradox Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the textualist-paradox reading — of
 *   the contested kernel 'all men are created equal.' This reading holds that
 *   the universal language of the founding text is structurally
 *   irreconcilable with the founding society's restricted application of it,
 *   and that this irreconcilability is itself the operative interpretive
 *   fact: a performative contradiction that delegitimizes any authority claim
 *   resting on founder intent as the boundary of the text's scope. This is
 *   distinct from the originalist reading (which reads the restriction as the
 *   text's true intended scope) and the universalist reading (which reads the
 *   universal language as a principle requiring iterative expansion
 *   independent of what founders intended). Each reading is a separate
 *   constraint with its own ε, beneficiaries, and victims — this file covers
 *   only the paradox reading.
 *
 * KEY AGENTS:
 *   - expansive_rights_litigants: beneficiary — gain textual leverage independent of founder-intent argument
 *   - critical_legal_scholars: beneficiary — build doctrine and career capital on the identified contradiction
 *   - originalist_interpretive_framework: payer — loses interpretive legitimacy as the contradiction is formalized
 *   - founding_era_authority_claimants: payer — bear reputational and doctrinal cost of defending restricted scope against a textual (not merely historical) challenge
 *   - current_judiciary: agenda_setter — administers which reading receives institutional force
 *   - excluded_historical_populations: excluded — the actual referents of the restriction, absent from the current interpretive contest
 *   - constitutional_law_analysts: observer — documents the structure of the paradox without adjudicating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.52).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist-Paradox Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, 'b349f47f-f450-4d46-8dc0-c78b6d11c295').
narrative_ontology:cs_kernel_codification('b349f47f-f450-4d46-8dc0-c78b6d11c295', fixed_text).
narrative_ontology:cs_authority_grounding('b349f47f-f450-4d46-8dc0-c78b6d11c295', distributed).
narrative_ontology:cs_reading_relation('b349f47f-f450-4d46-8dc0-c78b6d11c295', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('b349f47f-f450-4d46-8dc0-c78b6d11c295', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('b349f47f-f450-4d46-8dc0-c78b6d11c295', foundational, universal_text_performatively_self_undermining_when_restrictively_applied).
narrative_ontology:cs_axiom_status(universal_text_performatively_self_undermining_when_restrictively_applied, holdable).
narrative_ontology:cs_axiom_grounding('b349f47f-f450-4d46-8dc0-c78b6d11c295', universal_text_performatively_self_undermining_when_restrictively_applied, conventional).
narrative_ontology:cs_axiom('b349f47f-f450-4d46-8dc0-c78b6d11c295', secondary, founder_intent_cannot_narrow_unambiguous_universal_predicate).
narrative_ontology:cs_axiom_status(founder_intent_cannot_narrow_unambiguous_universal_predicate, holdable).
narrative_ontology:cs_axiom_grounding('b349f47f-f450-4d46-8dc0-c78b6d11c295', founder_intent_cannot_narrow_unambiguous_universal_predicate, conventional).
narrative_ontology:cs_reference_frame('b349f47f-f450-4d46-8dc0-c78b6d11c295', founding_text_as_written_1776).
narrative_ontology:cs_drift_state('b349f47f-f450-4d46-8dc0-c78b6d11c295', contemporary_civil_rights_jurisprudence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b349f47f-f450-4d46-8dc0-c78b6d11c295', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, expansive_rights_litigants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, critical_legal_scholars).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, founding_era_authority_claimants).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, performative_contradiction_thesis).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, textual_universality_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates and plaintiffs invoking the plain universal text against restricted historical application gain a textual lever: the document's own words, not contested founder biography, become the argumentative ground. They benefit from the paradox reading because it does not require winning an argument about what founders privately believed — it only requires reading the sentence aloud next to the franchise rolls of 1776.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, expansive_rights_litigants, beneficiary,
    moderate, generational, mobile, national).

% Academics who build careers and doctrine on demonstrating the gap between founding text and founding practice. The paradox reading is their primary analytical instrument; it converts a historical embarrassment into a permanent structural critique that generates ongoing scholarship, citations, and doctrinal leverage.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, critical_legal_scholars, beneficiary,
    organized, civilizational, arbitrage, national).

% The interpretive tradition itself is not a person but a doctrine-bearing structure with real institutional carriers (see founding_era_authority_claimants). As a framework, it cannot 'exit' the contradiction — its legitimacy depends on treating founder intent as authoritative, and the paradox reading is structurally engineered to make that intent look either hypocritical or narrower than the text it authored. It bears the cost of the reading's success as a loss of interpretive authority, not as a resource loss.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).

% Jurists, politicians, and commentators whose authority rests on claiming fidelity to founders' original scope (property-holding white male citizens as the operative referent of 'men'). The paradox reading does not merely disagree with them — it names their position as the performative contradiction, undercutting the legitimacy of citing founder intent at all unless they first explain why universal language was used insincerely or narrowly. They cannot cheaply exit this bind: abandoning originalism costs professional and ideological capital; defending it require rebutting a textual observation, not a factual one.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, founding_era_authority_claimants, payer,
    powerful, generational, constrained, national).

% Courts and constitutional interpreters must decide which reading of the founding language governs a given case. They administer which interpretive frame gets institutional force, and the paradox reading pressures them toward treating universal text as controlling over restricted historical practice — but they also bear political cost for appearing to abandon originalist legitimacy narratives entirely.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, current_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% The enslaved, women, and non-propertied men excluded from the founding era's practical application of 'all men are created equal' have no voice in either the founding debate or, largely, in whose interpretive framework wins today — they are the historical referent the paradox reading is ABOUT, but the fight over how to read the contradiction is conducted primarily among later interpretive communities, not by them.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_historical_populations, excluded,
    powerless, generational, trapped, national).

% Legal historians and philosophers of law who examine the structural properties of the contradiction itself — neither defending originalism nor advocating for universalist expansion, but documenting how the paradox reading operates as an interpretive move with its own winners and losers.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_law_analysts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The paradox reading solves a genuine interpretive coordination problem: it gives later courts and advocates a textually disciplined method for resolving the gap between founding language and founding practice without requiring speculative reconstruction of founders' private mental states — the text's own internal tension becomes the operative fact.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from the originalist framework (and those whose institutional position depends on it) to those who can invoke the universal text against restricted historical practice — a transfer of argumentative leverage and doctrinal capital, not money.
% ABSENT_VOICES: The enslaved and disenfranchised populations who were the actual referents of the restricted application are not present in the interpretive contest at all; the debate over how to read their exclusion is conducted entirely among later actors (originalists, textualist-paradox theorists, universalists) none of whom were there.
% DISAPPEARANCE_RATIONALE: If the paradox reading were withdrawn from circulation, originalist claims to founder-sanctioned restricted scope would face substantially less structural pressure, litigation strategies built on textual universality would lose a key doctrinal tool, and the interpretive contest over the equality clause would revert to a dispute more heavily weighted toward historical-intent evidence rather than internal textual contradiction.
% FOUNDING_PROBLEM: The founding text used maximally universal language ('all men are created equal') while the founding society practiced maximally restricted application (chattel slavery, coverture, property qualifications) — the paradox reading was built to name and formalize this gap as a structural feature of the constitutional kernel rather than an accident of history.
% FOUNDING_PROBLEM_CORROBORATION: The gap between universal text and restricted practice is independently attested by historians of the founding era (documented voting rolls, slave codes, coverture statutes) who are outside both the originalist and the critical-legal-scholar camps that benefit from competing readings of it — the underlying empirical gap is not contested; only its interpretive consequence is.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at present) because this reading does not merely add descriptive nuance — it actively delegitimizes a rival interpretive authority claim (originalism) by naming its foundational move as a performative contradiction. That is a real transfer of legitimacy capital, not neutral commentary. Suppression is lower (0.38) because the paradox reading operates through argument and citation, not coercive enforcement — its force is rhetorical-doctrinal, propagated through courts and scholarship rather than police power. Theater ratio is modest and rising (0.10 to 0.28) reflecting some accumulation of citation-ritual (the paradox move becoming a rhetorical set-piece in briefs and opinions) without displacing its substantive analytical function. Resistance is high (0.71) because originalist-aligned jurists and scholars actively contest the paradox framing rather than yielding to it.
 *
 * PERSPECTIVAL GAP:
 *   From the critical-legal-scholar seat, this reading is a coordination achievement: it resolves interpretive ambiguity by locating authority in the text's own internal structure rather than contested historical psychology. From the originalist-authority seat, the identical structural move reads as extraction — a delegitimizing maneuver dressed as textual analysis, which the engine should register as active enforcement (courts citing the paradox to override intent-based scope arguments) rather than pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive rights litigants and critical legal scholars are declared beneficiaries because the paradox reading supplies them a textually disciplined tool that does not require winning a historical-intent argument — this is a genuine directional gain, feeding low d. The originalist interpretive framework and its institutional carriers (founding-era authority claimants) are declared victims because the reading's entire structural purpose is to expose their foundational move as contradictory — feeding high d, particularly for authority claimants who cannot exit the argument cheaply (their professional and ideological capital is invested in exactly the position being undercut).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the universal-text/restricted-practice gap) remains live — it has not been resolved by subsequent constitutional amendment or social consensus, it has only been repeatedly renegotiated. This is why founding_problem_status is 'live' rather than 'dead': the paradox reading has not aged into obsolete scaffolding: it continues to do active interpretive work each time restricted-scope authority claims are advanced, which is precisely the enforcement mechanism keeping this a tangled_rope rather than a settled mountain or an abandoned piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradox_reading_kernel_instability,
    'Is the performative contradiction identified by this reading a discovery about the kernel''s actual internal structure, or is it itself an interpretive construction that could be dissolved by a different framing (e.g., treating ''men'' as a term of art with a settled 18th-century referent, dissolving the apparent contradiction rather than exposing it)?',
    'Comparative textual-historical analysis of contemporaneous usage of universal terms in 18th-century legal and philosophical documents — if ''all men'' consistently functioned as an unmarked term of art excluding certain groups across the genre, the paradox may be a modern retrospective construction rather than an internal contradiction the founders themselves would have recognized as contradictory.',
    'If the contradiction is a genuine structural feature of the text, this reading''s delegitimizing force against originalism is well-grounded. If it is substantially a modern interpretive artifact, the extraction this reading performs against originalist authority claims is less warranted than the metrics suggest, and part of the measured extractiveness reflects rhetorical effectiveness rather than structural discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_reading_kernel_instability, conceptual, 'Whether the performative contradiction is discovered in the kernel or constructed by this reading''s framing.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the textualist-paradox reading merely coexist with the originalist reading (as competing interpretive communities), or does it structurally foreclose originalism by making the founder-intent move logically self-undermining wherever universal language is used insincerely?',
    'Track whether originalist jurisprudence, when confronted with the paradox argument in briefs and opinions, responds by (a) continuing to assert founder-intent scope unmodified (coexistence — the paradox has not foreclosed anything), or (b) is forced into secondary moves like disclaiming the universal language''s sincerity or redefining ''men'' as always-already restricted (which would suggest partial foreclosure — the pure originalist position becomes harder to hold without modification).',
    'If (a), the relation to originalist_reading in cs_structure should remain coexists_with. If courts and scholars increasingly show pattern (b), a future revision might warrant reclassifying the relation toward influences or even forecloses for specific sub-claims within originalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Whether the paradox reading logically forecloses pure originalism or merely competes with it.').

omega_variable(
    victim_status_of_a_framework,
    'Can an interpretive framework (originalist_interpretive_framework) coherently be a ''victim'' in the structural sense, or does declaring a doctrine as victim smuggle in an implicit claim that the doctrine deserves protection independent of its truth-value?',
    'Distinguish the doctrine (which bears no welfare) from its institutional carriers (founding_era_authority_claimants, who do bear reputational and career costs) — this story declares both, with the framework itself marked agent:false precisely to avoid attributing welfare to a non-agent while still tracking the structural cost to the interpretive position.',
    'If the framework-as-victim framing is judged incoherent, the base_properties.victims entry for originalist_interpretive_framework should be removed and the extraction analysis should rest solely on founding_era_authority_claimants as the bearer of cost — this would likely lower measured extractiveness slightly, since a diffuse doctrinal cost is folded into a concentrated human-actor cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_a_framework, conceptual, 'Whether a non-agent interpretive framework can coherently be named a structural victim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1776, 0.1).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1868, 0.28).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1920, 0.34).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2026, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(all_men_created_equal__textualist_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of a single contested kernel (all_men_created_equal). The originalist_reading treats the restriction as the text's true intended scope (low extraction against expansive claimants, high stability for founder-authority claims). The universalist_reading treats the universal language as an independent normative principle warranting expansion regardless of intent (its own distinct extraction profile, likely lower suppression but higher accessibility_collapse against restrictive counter-readings). This textualist_paradox_reading sits structurally between them: it does not assert the universalist principle affirmatively, but it does actively undermine the originalist authority claim by naming the text's internal contradiction. Each reading carries its own ε and stakeholder structure; none should be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
