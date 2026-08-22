% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property Rights Reading)
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   Fair use doctrine under U.S. copyright law is statutorily open-ended but
 *   judicially interpreted. This constraint instantiates ONE reading:
 *   copyright is fundamentally property; fair use is a narrow affirmative
 *   defense that must not undermine the licensing market. Under this reading,
 *   courts treat commercial use as nearly presumptively infringing,
 *   underweight transformative purpose, and place the burden on defendants to
 *   prove their use fits narrow carve-outs (criticism, news reporting,
 *   scholarship, parody in limited forms). The kernel (Section 107 of the
 *   Copyright Act) is fixed text, but its interpretation is contested. This
 *   reading treats the licensing market as the value to preserve and fair use
 *   as an exception to that preservation, not as a coequal purpose. The
 *   metric trajectory shows rising extractiveness (courts tightened fair use
 *   over 1976–2024) and rising theater (an increasing share of enforcement
 *   activity defends licensing-market interests rather than addressing actual
 *   public-domain exhaustion problems).
 *
 * KEY AGENTS:
 *   - Copyright holders: institutional beneficiaries controlling reproduction rights; justify high licensing fees as necessary to fund creation and distribution.
 *   - Commercial licensing market: the arrangement itself that this reading protects; publishers of textbooks, licensing agencies (CCC), music rights organizations (ASCAP/BMI).
 *   - Unauthorized secondary users: moderate-power payers who need to reuse copyrighted material; face licensing costs or litigation risk.
 *   - Transformative creators: identity-locked payers whose artistic practice IS unauthorized reuse; under this reading they must either license or lose their voice.
 *   - Academic researchers: constrained payers conducting analysis on copyrighted texts; face licensing fees or self-censorship.
 *   - Courts: institutional agenda-setters administering this interpretive frame; apply the narrow-defense logic to specific cases.
 *   - Excluded cultural commons: powerless, trapped voices — remix artists, vernacular creators, Global South adapters — who would benefit from broader fair use but cannot litigate and are not represented in shaping precedent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property Rights Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'a60d15e1-af72-4e26-95d3-4d74a99a0565').
narrative_ontology:cs_kernel_codification('a60d15e1-af72-4e26-95d3-4d74a99a0565', fixed_text).
narrative_ontology:cs_authority_grounding('a60d15e1-af72-4e26-95d3-4d74a99a0565', lineage).
narrative_ontology:cs_interpretation_layer_present('a60d15e1-af72-4e26-95d3-4d74a99a0565').
narrative_ontology:cs_reading_relation('a60d15e1-af72-4e26-95d3-4d74a99a0565', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_reading_relation('a60d15e1-af72-4e26-95d3-4d74a99a0565', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_axiom('a60d15e1-af72-4e26-95d3-4d74a99a0565', foundational, copyright_is_property_right).
narrative_ontology:cs_axiom_status(copyright_is_property_right, holdable).
narrative_ontology:cs_axiom_grounding('a60d15e1-af72-4e26-95d3-4d74a99a0565', copyright_is_property_right, deontological).
narrative_ontology:cs_axiom('a60d15e1-af72-4e26-95d3-4d74a99a0565', foundational, licensing_market_preservation_justifies_exception).
narrative_ontology:cs_axiom_status(licensing_market_preservation_justifies_exception, holdable).
narrative_ontology:cs_axiom_grounding('a60d15e1-af72-4e26-95d3-4d74a99a0565', licensing_market_preservation_justifies_exception, instrumental).
narrative_ontology:cs_reference_frame('a60d15e1-af72-4e26-95d3-4d74a99a0565', copyright_property_licensing_market_primacy).
narrative_ontology:cs_drift_state('a60d15e1-af72-4e26-95d3-4d74a99a0565', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a60d15e1-af72-4e26-95d3-4d74a99a0565', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, commercial_licensing_market).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, unauthorized_secondary_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, academic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the exclusive right to reproduce and distribute copyrighted works. Under this reading, fair use is a narrow exception that exists to prevent absurd outcomes (quoting a sentence in a review), not to enable secondary markets or transformative reuse. They enforce the right by litigating against unauthorized uses, demanding licensing fees, and shaping court precedent toward market-preservation outcomes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, global).

% The institutional arrangement by which copyright holders monetize permissions to reuse works. This reading treats the licensing market as the primary value to be protected; any use that could be licensed is presumptively infringing unless the defendant can prove their use falls within the narrow carve-outs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, commercial_licensing_market, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(fair_use_statutory_exception__narrow_defense_reading, commercial_licensing_market).

% Desire to reuse copyrighted material — quoting excerpts, sampling audio, adapting plots, creating mashups, conducting text analysis on published works — without first obtaining (and paying for) explicit permission. This reading's doctrine places the burden on them to prove their use qualifies as fair use through narrow statutory factors. Most face either licensing costs, self-censorship, or litigation risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, unauthorized_secondary_users, payer,
    moderate, biographical, constrained, global).

% Create works that build on, critique, parody, or sample existing copyrighted material as a core part of their artistic practice. Under this narrow reading, their work is presumptively infringing unless they can fit within narrow carve-outs; the reading underweights the transformative nature of their contribution and instead emphasizes market harm to licensing. They face identity-lock because their creative practice IS the unauthorized reuse; exit means abandoning their artistic voice.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, transformative_creators, payer,
    moderate, biographical, identity_locked, global).

% Conduct research on copyrighted texts — textual analysis, computational linguistics, literary criticism — that requires reproduction and redistribution of excerpts or data. This reading's doctrine treats academic use as commercially competitive with licensing arrangements; they face licensing costs or self-censorship to avoid litigation risk.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, academic_researchers, payer,
    moderate, biographical, constrained, global).

% Interpret the fair use statute and apply it to specific cases. This reading constitutes a coherent interpretive frame: copyright is property first, the licensing market is the value to preserve, and fair use is a narrow exception. Courts following this frame weigh commercial nature heavily, skepticism of transformativeness, and market harm narrowly (to licensing arrangements specifically). They administer the constraint by accepting this framing and rejecting broader readings of fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts_applying_fair_use, agenda_setter,
    institutional, generational, analytical, national).

% Set the statute that authorizes fair use (Section 107 of the Copyright Act). Congress left the fair use doctrine deliberately flexible and fact-dependent; this reading is one way to instantiate that flexibility, crystallizing it into a property-preserving interpretation. Congress could amend the statute to mandate different outcomes (e.g., explicit transformative-use immunity, research exemptions), but has not.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, congress, observer,
    institutional, generational, analytical, national).

% Parties who would benefit from broader fair use doctrine (remix artists, fan communities, vernacular creators, cultural adapters in the Global South) but are structurally excluded from the commercial licensing market and from litigation resources. They are not parties to court cases, not represented in legislative debate, and face the most restrictive interpretation of fair use without recourse. Their exclusion is not incidental — it is built into who gets to define 'market harm' and who bears the burden of proof.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, excluded_cultural_commons, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes the right to control and monetize published creative works through an orderly licensing market: authors/publishers set prices, would-be reusers pay, both parties benefit from the certainty and predictability of the arrangement.
% TRANSFER_FUNCTION: Moves licensing revenue from secondary users to copyright holders; transfers decision-making authority over reuse from potential creators/public to copyright holders; restricts who can reuse published works without paying for permission.
% ABSENT_VOICES: Excluded cultural commons — remix communities, vernacular creators, fan communities, academics and researchers in resource-constrained contexts, adapters and oral-tradition keepers in the Global South — would argue for broader fair use as a public interest, not merely a market-preserving exception. They have no litigation resources, no representation in courts that shape precedent, and no voice in the legislative process that could amend the statute. The narrow reading is authored and defended by copyright holders, courts, and commercial licensing organizations; the voices that would benefit from broader fair use are systematically excluded from authority.
% DISAPPEARANCE_RATIONALE: If the narrow-defense reading disappeared and fair use doctrine expanded to emphasize transformative use and cultural benefit, secondary creators would face lower litigation risk, licensing markets might contract but cultural production would accelerate, remix and sampling practices would flourish openly, and courts would shift burden of proof toward copyright holders. The licensing market would reorganize around fewer, higher-value premium licenses and more uncompensated transformative reuse. Academic researchers could conduct text analysis without licensing concerns. Fan communities and remix cultures could operate openly rather than in legal gray zones.
% FOUNDING_PROBLEM: Copyright monopolies can be absolute; without a safety valve, owners could forbid any quotation, critique, or adaptation, choking off the public interest in accessing and building on published knowledge. Fair use was created to prevent absurd outcomes where copyright owners could suppress all critical speech about their work.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and courts applying the market-preservation frame argue the founding problem (absolute monopoly power) is not live because fair use exists as a narrow carve-out, and that carve-out is sufficient. They cite the narrow exceptions (criticism, scholarship, news reporting, parody) as adequate safety valves. Academic copyright scholars, transformative creators, and advocates for open culture argue the founding problem IS live under this narrow reading — that the reading has collapsed fair use into a licensing-market appendage and abandoned its broader purpose. Legislative history from the 1976 Copyright Act shows Congress intended fair use to be broad and fact-dependent, not narrow. Evidence from international comparisons shows jurisdictions with more generous transformative-use doctrine (European Union, Canada) have vibrant licensing markets and higher levels of cultural production, suggesting the narrow reading conflates market-preservation with market-viability. The Sony Betamax case (1984) and Campbell v. Acuff-Rose case (1994) were read as supporting the market-preservation frame, but dissenting opinions and later scholarship argue the cases are consistent with broader fair use. The founding problem's status is genuinely contested; no single corroborating authority settles it.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at 2024) because this reading collapses the scope of fair use into a licensing-market appendage: most unauthorized uses are presumptively infringing unless the defendant proves fit within narrow carve-outs. Suppression is high (0.71) because the constraint's persistence depends on active litigation enforcement (copyright holders sue heavily) and on rejecting broader interpretations of fair use that would reduce licensing demand. Theater is moderate (0.28) — the courts do apply a genuine statutory test, but an increasing share of that test's weightings (commercial nature, market harm) serve licensing-market interests rather than addressing the statute's broader purposes (enabling criticism, scholarship, cultural production). The measurement series show monotonic increase: as digital distribution made copying costless and as licensing organizations (CCC, ASCAP) built out licensing infrastructure, the narrow reading gained institutional support and courts tightened fair use doctrine over the 1976–2024 interval. Rising theater reflects the shift from genuine carve-out (which fair use was in the 1970s) toward instrumentalized licensing defense (which it has become). The interval starts at the 1976 Copyright Act codification of fair use; the endpoint is 2024.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder seat and the court seat should compute differently from the payer seats. Copyright holders perceive the narrow-defense reading as a natural (even generous) accommodation of property rights; the narrow exceptions are all the public should need. Transformative creators and secondary users perceive the same reading as extractive suppression; fair use has been gutted and the licensing market is the beneficiary. The engine computes this divergence from structural data: copyright holders have high exit options (can always license, can sue, can lobby for stronger rights) and collect from the constraint; transformative creators have identity-locked exit and pay the constraint. Courts sit as agenda-setters, shaping the frame itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are beneficiaries (d near 0.0): they collect licensing revenue, control reuse decisions, set market prices. Unauthorized secondary users and transformative creators are targets (d near 1.0): they pay licensing costs or face litigation risk and must prove their use qualifies as fair use (burden on them). Courts are agenda-setters (d = 0.5): they apply a doctrine that serves copyright-holder interests but use real statutory language; the power asymmetry is structural, not personal. The licensing market is not an agent, but a non-agent beneficiary entity tracking the institutional arrangement that this reading protects.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows mandatrophy: the founding problem (absolute copyright monopoly with no escape valve) was live in 1976 when fair use was codified as a genuinely open carve-out. By 2024 the founding problem is dead (fair use exists as stated) but the arrangement persists as a licensing-market defensive mechanism rather than as a public-interest exception. Courts have narrowed fair use doctrine over 48 years, not because the statute changed, but because the reading crystallized into property-preserving interpretation. The measurement trajectory (rising extractiveness, rising theater) tracks this mandatrophic drift: the constraint has traded its original purpose for rent collection. The constraint is tangled_rope, not rope, because it claims coordination (fair use as necessary accommodation) while actually extracting (licensing fees, suppressed transformative reuse) — both present, both structural, both enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_vs_substitution_boundary,
    'How should courts distinguish transformative use (adding new purpose or meaning to the original work) from mere substitution (copying the original without significant modification)?',
    'Case law clustering analysis: map holdings in transformative-use cases; if clustering emerges around judicial factors, the boundary exists in practice; if holdings remain scattered, the boundary is incoherent and reading-dependent.',
    'A clear boundary would support fair use claims in genuine transformation (criticism, scholarship, parody) and reduce litigation burden on secondary creators. The narrow reading underweights transformation; a clearer boundary could shift doctrine. If the boundary remains incoherent, the narrow reading''s discretionary application becomes evident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_vs_substitution_boundary, empirical, 'Whether transformative use has a principled boundary or remains reading-dependent.').

omega_variable(
    market_harm_scope_ambiguity,
    'What counts as relevant market harm under fair use analysis: only harm to licensing markets for the original work, or broader harms (e.g., reduced sales of the original itself)?',
    'Statutory history and legislative record review; empirical analysis of jurisdictions applying different market-harm standards; economic analysis of substitution vs. complementarity effects.',
    'The narrow reading treats market harm narrowly (licensing-market substitution is harm; cultural benefit does not offset it). A broader reading would count net effects (does the secondary use increase or decrease demand for the original?). This ambiguity is where the reading does most work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_scope_ambiguity, conceptual, 'The scope of market harm the fair use test should consider.').

omega_variable(
    commercial_nature_determinism,
    'Should commercial use of a copyrighted work be treated as nearly presumptively unfair, or should the test weigh all four factors equally regardless of commerciality?',
    'Doctrinal analysis of Supreme Court precedent (Sony, Harper & Row, Campbell v. Acuff-Rose) and circuit split resolution; international comparison with jurisdictions that weight commercial nature less heavily.',
    'Under this reading, commercial nature is determinative — a commercial use must clear a high bar to qualify as fair use. The transformative-right reading treats commercial nature as one factor among four, not a dispositive gate. This is a core point of disagreement between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_nature_determinism, conceptual, 'The weight commercial nature should carry in fair use analysis.').

omega_variable(
    identity_lock_mechanism,
    'For transformative creators (sampling musicians, remix artists, adaptors), how far is their creative identity fused with unauthorized reuse? What would exit look like — artistic abandonment or artistic redirection?',
    'Ethnographic study of remix/sampling communities; post-licensing interviews with creators about identity impact; comparison with creators who fully licensed vs. self-censored vs. litigated.',
    'If identity fusion is strong, exit_options for transformative creators is truly identity_locked, and the constraint''s suppression is higher (they cannot leave without abandoning their voice). If fusion is weak, exit is constrained but not identity-locked, and suppression is lower. This affects classification at the creator seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The degree of identity fusion between transformative creators and their reuse practice.').

omega_variable(
    reading_kernel_committer_ambiguity,
    'Is the narrow-defense reading a coherent instantiation of fair use doctrine, or is it an interpretive innovation that leans toward market-licensing reading by stretching the statute''s language?',
    'Textual analysis of Section 107; legislative history tracing from the 1976 Act''s passage through key Supreme Court decisions (Sony, Harper & Row, Campbell); comparison with the statute''s language and original public meaning.',
    'If the narrow reading is a coherent interpretation, courts are making legitimate doctrine-shaping choices; if it innovates beyond the statute, the reading should be classified as judicial amendment rather than interpretation. This affects the authority_grounding in cs_structure and the legitimacy of the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_committer_ambiguity, conceptual, 'Whether the narrow-defense reading is a statutory interpretation or a judicial innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.52).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fair_su_t2018, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).

% DUAL FORMULATION NOTE:
% The fair_use_statutory_exception kernel decomposes into three structurally distinct readings: narrow_defense_reading (this story) treats fair use as a narrow carve-out to preserve licensing markets (ε = 0.78, tangled_rope); market_licensing_reading would collapse fair use entirely (ε ≈ 0.95, snare); transformative_right_reading treats fair use as a coequal purpose enabling cultural production (ε ≈ 0.25, rope). The three readings instantiate different constraint values despite referencing the same statute. This story is the narrow reading; it influences both siblings by constraining transformativeness and defending licensing-market preservation as the reading's core logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
