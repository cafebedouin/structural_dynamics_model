% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public Enrichment Scaffold (Constitutional Reading)
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Copyright
 *   Clause kernel. The public_scaffold_reading frames copyright as a
 *   temporary grant of exclusive rights whose purpose is to enrich the public
 *   domain — monopoly is justified *only* as a means to that end, not as an
 *   end in itself. Copyright protection is the *scaffold*: temporary
 *   structure supporting the creation of works that will eventually enter the
 *   commons. This reading directly contests the corporate_enclosure_reading
 *   (copyright as perpetual property right) and sits in structural tension
 *   with the judicial_ambiguity_reading (which defers term-length decisions
 *   to legislative discretion without policing whether they honor the
 *   'limited times' mandate). The reading is authored from the seat of
 *   constitutional text-originalism, progressive copyright scholarship, and
 *   public-domain advocacy. Low extractiveness reflects that under this
 *   reading the system is primarily coordinating creation + eventual public
 *   access, not extracting rents. Moderate suppression reflects that
 *   copyright industries actively lobby against fair use expansion and term
 *   shortening — they must suppress certain policy moves to maintain the
 *   current regime.
 *
 * KEY AGENTS:
 *   - public_domain: the repository beneficiary; copyright serves this agent's accumulation
 *   - initial_creators: protected for a term; the reading treats their protection as instrumental to public enrichment
 *   - downstream_creators: benefit from fair use and eventual public-domain access; this reading prioritizes their creative freedom
 *   - educational_institutions: benefit from public domain and fair use provisions; rely on access for mission
 *   - legislative_authority: holds constitutional mandate to set 'limited times'; the reading interprets this as an obligation to police term length
 *   - copyright_industries: excluded; would argue for perpetual or near-perpetual protection
 *   - courts: observer seat; measure compliance with public-enrichment mandate
 *   - international_trade_regimes: excluded; would impose minimum protections that conflict with the public-scaffold mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.28).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.15).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public Enrichment Scaffold (Constitutional Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '04a2092a-1202-43a2-a8ef-ea76f6eee894').
narrative_ontology:cs_kernel_codification('04a2092a-1202-43a2-a8ef-ea76f6eee894', fixed_text).
narrative_ontology:cs_authority_grounding('04a2092a-1202-43a2-a8ef-ea76f6eee894', lineage).
narrative_ontology:cs_interpretation_layer_present('04a2092a-1202-43a2-a8ef-ea76f6eee894').
narrative_ontology:cs_reading_relation('04a2092a-1202-43a2-a8ef-ea76f6eee894', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('04a2092a-1202-43a2-a8ef-ea76f6eee894', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('04a2092a-1202-43a2-a8ef-ea76f6eee894', foundational, limited_times_is_binding_constraint).
narrative_ontology:cs_axiom_status(limited_times_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('04a2092a-1202-43a2-a8ef-ea76f6eee894', limited_times_is_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('04a2092a-1202-43a2-a8ef-ea76f6eee894', foundational, public_domain_accumulation_is_primary_mandate).
narrative_ontology:cs_axiom_status(public_domain_accumulation_is_primary_mandate, holdable).
narrative_ontology:cs_axiom_grounding('04a2092a-1202-43a2-a8ef-ea76f6eee894', public_domain_accumulation_is_primary_mandate, deontological).
narrative_ontology:cs_reference_frame('04a2092a-1202-43a2-a8ef-ea76f6eee894', limited_times_as_constitutional_mandate).
narrative_ontology:cs_drift_state('04a2092a-1202-43a2-a8ef-ea76f6eee894', contemporary_copyright_era_post_1976, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04a2092a-1202-43a2-a8ef-ea76f6eee894', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, educational_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, initial_creators).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, copyright_as_means_not_end).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, temporary_monopoly_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_enrichment_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The repository of works that have entered the commons or were never subject to copyright restriction. Under this reading, the entire copyright system exists to transition works into the public domain as its primary purpose — copyright protection is the *means*, public enrichment is the *end*. Public domain accumulation is the success metric of the system.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Authors, composers, filmmakers, and other original creators who receive temporary exclusive rights to their work under copyright. This reading frames their protection as a means to incentivize creation that will enrich the public domain, not as an end-in-itself property right. The temporary nature and eventual reversion to the public domain is *the point*, not a limitation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, initial_creators, beneficiary,
    moderate, biographical, constrained, global).

% Artists, writers, and builders who build on, remix, reinterpret, or reference earlier works. This reading prioritizes their access to the public domain as a foundational creative resource. They benefit from fair use, shorter copyright terms, and the eventual expiration of protections that would otherwise lock in perpetual restrictions on transformative work.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators, beneficiary,
    moderate, biographical, constrained, global).

% Universities, schools, libraries, and research institutions that serve public knowledge transmission. They depend on access to a robust public domain and fair use provisions to educate students, preserve knowledge, and enable scholarship without permission-seeking or licensing fees that would fragment knowledge access by ability to pay.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Congress holds constitutional power to 'promote the progress of science and useful arts by securing for limited times' exclusive rights. This reading holds that 'limited times' is the operative constraint — a mandate, not permission. Legislature sets the term length as the primary lever; the reading interprets shorter terms and stronger fair use as fidelity to the mandate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, legislative_authority, agenda_setter,
    institutional, generational, analytical, national).

% Entertainment, software, and publishing corporations that treat copyright as perpetual or near-perpetual property rights and lobby for term extension, anti-circumvention rules, and aggressive enforcement. This reading excludes their framing: they would argue copyright should maximize duration and exclude all derivative use. Their exclusion from agenda-setting in this reading is exactly the interpretive contest.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_industries, excluded,
    institutional, generational, trapped, global).

% Federal judiciary interprets copyright statutes and the 'limited times' clause. In this reading, courts have a duty to police term extension as anti-constitutional and to enforce fair use vigorously as a statutory mandate to preserve creative freedom. They observe and measure fidelity to the public-enrichment mandate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% Trade agreements (TRIPS, USMCA) that mandate minimum copyright protections and restrict signatories' ability to set their own copyright terms. This reading excludes their framework: TRIPS-mandated protections are treated as pressure toward enclosure, not as fidelity to copyright's constitutional purpose. The reading asserts that fidelity to the public-enrichment mandate may require breaking with international IP harmonization.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, international_trade_regimes, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Temporarily grants creators exclusive rights to incentivize production of original works that will eventually enter the public domain, enriching the cultural commons. The coordination problem: without some protection, creators lack incentive to produce; without a sunset, the protection becomes perpetual rent extraction. The system solves the incentive problem *while preserving* eventual public access.
% TRANSFER_FUNCTION: Transfers temporary exclusive control over creative works from the creator to themselves for a defined term, with the obligation that after that term the work must revert to public use. The arrangement moves the economic benefit of creation from creators (during the term) to society (after expiration). It also moves restriction-of-use from no-one-can-use to only-licensee-can-copy.
% ABSENT_VOICES: Copyright industries and international trade bodies that would argue for perpetual or near-perpetual protection are structurally excluded from this reading's authority structure. Downstream creators in the Global South, derivative artists, and public-knowledge advocates who face the severest access barriers are often absent from legislative bodies that set copyright terms. Their exclusion from the conversation shaped the current long-term regime.
% DISAPPEARANCE_RATIONALE: If copyright under this reading's mandate were enforced (shorter terms, stronger fair use, anti-enclosure norms), the literary and cultural commons would be orders of magnitude larger; derivative work, remix, adaptation, and scholarly reference would accelerate; educational costs would plummet. The world rearranges because the current regime suppresses creation that would thrive in a more open commons.
% FOUNDING_PROBLEM: How to incentivize the creation of literary, artistic, and scientific works while preserving the ability of future creators and the public to use, learn from, remix, and build upon those works. The founding problem assumes copyright is a *temporary* grant of exclusive rights, not a permanent property regime.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional text itself ('limited times') attests the problem; early copyright scholars and judges (1790s–1920s) endorsed limited terms as the animating principle. Modern scholarship in law and economics (Lessig, Boyle, Sunder, Samuelson) documents that technological and industry changes have stretched 'limited' into de facto perpetuity. Federal Register comments from libraries, educators, and digital creators attest the founding problem persists: the term is no longer limited in any practical sense, and the public domain is shrinking.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28 end-state) because under this reading the system's primary function IS coordination (create + eventually open), not rent extraction. The escalation from 0.18 to 0.35 mid-interval reflects that copyright industries have captured the regime over the past 20 years, stretching terms, narrowing fair use, and adding anti-circumvention rules — extractive pressure building as the corporate_enclosure_reading gains institutional power. The projected decline to 0.28 by interval end (t=40) reflects the assumption that either (1) legislative or judicial reform reasserts the public-enrichment mandate, or (2) technological change (AI, remix culture, decentralization) makes copyright enforcement so costly and culturally incoherent that enclosure fails. Theater rises mid-interval (0.08→0.3) as the regime increasingly justifies itself through rhetoric ('creator protection,' 'protecting American creativity') while the actual function shifts toward rent extraction; the projected decline at end reflects either successful reform (theater drops because the mandate is re-aligned) or regime collapse (theater becomes irrelevant). Suppression is low because this reading does not require coercive enforcement of participation — creators voluntarily participate in copyright; the suppression that does exist (copyright industries suppressing fair use litigation, lobbying against term shortening) is structural resistance to reform, not foundational to the system. Resistance is moderate-high (0.58) because downstream creators, educators, and technologists actively work around copyright restrictions (remix, fair use claims, decentralized platforms); the public_scaffold_reading legitimates their resistance as aligned with copyright's actual purpose.
 *
 * PERSPECTIVAL GAP:
 *   Payer/beneficiary asymmetry is minimal under this reading because the system is framed as coordination, not extraction. Initial creators pay nothing; they receive protection as an incentive. The 'victims' (if any) would be copyright industries under enforcement of the public-enrichment mandate — but they are excluded rather than victimized, because the reading denies them standing in the constitutional framework. Educational institutions and downstream creators experience moderate constraints (fair use limitations, licensing costs) but the reading re-frames these as *policy failures* (violations of the public-enrichment mandate) rather than as features of the system. The gap in perception arises from whether copyright terms are viewed as *limited* (satisfying the mandate) or *unlimited* in practical effect (violating the mandate). Initial creators in the corporate_enclosure_reading would perceive themselves as targets of public-domain obligations; under the public_scaffold_reading they perceive themselves as beneficiaries of incentive protection, with no sense of loss when their works eventually enter the commons.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality diverges sharply across seats. Initial creators sit near d=0.3 (modest beneficiary: they get protection, but it is temporary and the reading treats their protection as instrumental, not an end). Downstream creators and educational institutions sit near d=0.0 (beneficiaries: they gain from fair use expansion and public domain growth). The public_domain itself sits at d=0.0 (pure beneficiary: the reading exists to serve it). Copyright industries would sit near d=1.0 (targets: they are suppressed by term limits, fair use, and public-domain obligations), but they are excluded from the reading's authority structure, so they do not seat. Legislative authority sits near d=0.5 (symmetric: they must balance incentive-for-creation against public-enrichment, a genuine symmetric tension). Courts sit at d=analytical (observer). Notably, this reading produces a *different* directionality vector than the corporate_enclosure_reading would — that reading would seat copyright industries as beneficiaries (near d=0.0 from their perspective) and downstream creators as targets (d=1.0). The divergence in directionality across readings is exactly the kernel contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint EXHIBITS mandatrophy in the historical record: the founding problem (how to incentivize creation while preserving public access) was live until roughly 1976 (Copyright Term Extension Act era). After 1976, Disney and other corporations successfully lobbied to extend terms multiple times, stretching 'limited times' from ~56 years to ~120 years. The *founding mandate* — the 'limited times' requirement — has been neutered by legislative capture. This reading's response is NOT to declare the constraint dead, but to insist the mandate is LIVE and has been *violated*. The theater_ratio escalation (0.08→0.3) reflects that copyright industries increasingly defend long terms through cover stories (creator protection, American competitiveness) while the actual function has shifted to rent extraction. The reading treats this as *degradation* of the system, not as natural evolution. If term extension continues unabated, the constraint becomes a piton: the public-enrichment mandate remains written in the statute and Constitution, but is performed theatrically without real effect. The reading's projected recovery (theater decline at t=40) reflects either (1) successful reform that re-aligns practice with mandate, or (2) regime collapse where copyright becomes too costly to enforce. Mandatrophy is CONTESTED (the corporate_enclosure_reading denies mandatrophy exists — it reads copyright as a property right that can be extended indefinitely). The judicial_ambiguity_reading avoids the question by deferring to legislatures, sidestepping the constitutional mandate entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_meaning,
    'What does ''limited times'' mean as a constitutional constraint on copyright term-setting? Does it prohibit indefinite extension, or is it merely a rhetorical preference compatible with near-perpetual terms?',
    'Originalist historical analysis of founding-era copyright understanding and statutory practice. Comparative analysis of how ''limited'' has been interpreted in other constitutional contexts (e.g., ''reasonable'' in Fourth Amendment). Functional analysis of whether 120-year terms serve the Clause''s stated purpose (promote progress) or merely rent extraction.',
    'If ''limited times'' is a binding prohibition, term extension legislation violates the Constitution and courts must police it; the public_scaffold_reading holds. If ''limited'' merely expresses deference to legislative discretion, the corporate_enclosure_reading and judicial_ambiguity_reading both survive. This is the core omega for the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_times_meaning, conceptual, 'Constitutional meaning of ''limited times'' as a constraint on copyright terms.').

omega_variable(
    public_enrichment_mandate_vs_incentive_justification,
    'Does copyright''s constitutional purpose prioritize accumulation of a public domain (public enrichment as end-state), or does it permit copyright industries to retain works in perpetuity *provided* they are incentivizing creation?',
    'Analysis of whether ''promote the progress'' is satisfied by (a) maximum incentive regardless of public-domain size, or (b) a balance that prioritizes both incentive and eventual public access. Empirical study of whether term length affects creation incentives beyond a threshold (most evidence suggests 20 years suffices; additional years add minimal incentive but massive public-access delay).',
    'If public-domain accumulation is the end-state metric, term shortening is mandatory; the public_scaffold_reading''s scaffold claim holds. If incentive-maximization is the only metric, 120-year terms satisfy the Clause; the corporate_enclosure_reading''s perpetuity frame is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_enrichment_mandate_vs_incentive_justification, empirical, 'Whether copyright''s purpose is public-domain accumulation or maximum-incentive creation.').

omega_variable(
    capture_of_legislative_authority,
    'To what extent has copyright legislation been captured by copyright industries, causing deviations from the public-enrichment mandate that would not occur under uncaptured democratic process?',
    'Legislative history analysis: whose testimony, lobbying expenditure, and campaign contributions drive term extensions? Counterfactual: what copyright term would emerge from a legislative process dominated by public-domain advocates, downstream creators, and educators rather than Disney and music labels?',
    'If capture is severe, legislative term extensions do not reflect genuine democratic preference and courts should suspect constitutional violation; the public_scaffold_reading''s critique gains force. If capture is minimal, legislative outcomes reflect authentic public choice; the judicial_ambiguity_reading''s deference becomes appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_of_legislative_authority, empirical, 'Extent of industry capture of copyright legislative process.').

omega_variable(
    feasibility_of_public_enrichment_under_technology_change,
    'Can the public-enrichment mandate be enforced in an era of digital reproduction, remix culture, and international IP harmonization? Or does technological and geopolitical change make the scaffold unsustainable?',
    'Observation of how copyright is actually enforced and evaded in practice (DMCA circumvention, AI training on copyrighted data, decentralized platforms, piracy). Policy experiments with alternative incentive structures (shorter terms, strong fair use, public lending rights, creator grants).',
    'If the scaffold is unsustainable, the constraint becomes a piton—intellectually alive but practically inert. If alternatives emerge, the reading gains institutional plausibility. If industry enforcement hardens dramatically, suppression rises and the reading becomes contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_of_public_enrichment_under_technology_change, empirical, 'Whether the public-enrichment mandate can be institutionally sustained in the contemporary technological and geopolitical environment.').

omega_variable(
    suppression_mechanism_structural_vs_political,
    'Is the suppression measured (copyright industries blocking fair use expansion and term shortening) structural to copyright''s operation, or political opposition that could evaporate under different legislative coalitions?',
    'Observe whether suppression intensity correlates with copyright-industry lobbying expenditure (political) or with structural incentives inherent to copyright (e.g., does suppression persist even when copyright industries have no financial motivation to suppress?). Analyze whether post-reform scenarios show suppression decline.',
    'If structural, suppression should be authored as a stable feature of the constraint. If political, suppression is contingent and the projected decline in theater/suppression at t=40 is plausible. This affects the reading''s stability classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_political, empirical, 'Whether copyright-industry suppression of public-access reforms is structural or contingent on political coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t5, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(copy_tr_t5, observed).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(copy_tr_t10, observed).
narrative_ontology:measurement(copy_tr_t15, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(copy_tr_t15, observed).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(copy_tr_t20, projected).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(copy_tr_t30, projected).
narrative_ontology:measurement(copy_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(copy_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t5, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(copy_be_t5, observed).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(copy_be_t10, observed).
narrative_ontology:measurement(copy_be_t15, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(copy_be_t15, observed).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(copy_be_t20, projected).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement_basis(copy_be_t30, projected).
narrative_ontology:measurement(copy_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(copy_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(copy_su_t0, observed).
narrative_ontology:measurement(copy_su_t5, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement_basis(copy_su_t5, observed).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(copy_su_t10, observed).
narrative_ontology:measurement(copy_su_t15, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement_basis(copy_su_t15, observed).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(copy_su_t20, projected).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(copy_su_t30, projected).
narrative_ontology:measurement(copy_su_t40, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(copy_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__public_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% Copyright Clause kernel admits three readings instantiated as separate constraint stories. The public_scaffold_reading (this story) frames copyright as temporary scaffolding to build a public domain. The corporate_enclosure_reading frames copyright as perpetual property protection. The judicial_ambiguity_reading defers term-length authority to legislatures without constitutional policing. Each reading produces different beneficiary structures, directionality vectors, and terminal types. The kernel contest is structural, not resolvable by viewing copyright 'from another angle' — it is three genuinely distinct constraints sharing a contested constitutional text. The public_scaffold_reading forecloses corporate_enclosure on the question of term permanence; it influences judicial_ambiguity by asserting a constitutional floor on 'limited times'; it coexists with pragmatic democratic readings that accept any legislative choice as valid. Siblings linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__public_scaffold_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
