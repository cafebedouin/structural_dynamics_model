% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Licensing as Tiered Market Access Filter
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'licensing_statute_mandate': the 'graduated_access_filter' reading frames
 *   statutory credential requirements as a mechanism that sorts labor market
 *   access by class and prior resource access. The constraint is NOT about
 *   all statutory credentialing—it is about the structure created when
 *   credential requirements (nominally universal standards) are paired with
 *   fee barriers, unpaid apprenticeship periods, exam preparation costs, and
 *   time barriers that the resource-poor cannot absorb. The reading is one of
 *   three siblings: 'public_safety_coordination' frames credentialing as
 *   genuine consumer protection; 'rent_seeking_suppression' frames it as
 *   incumbent cartel behavior. This constraint describes the empirical
 *   pattern of CLASS SORTING that emerges when a formally universal standard
 *   is materially unequal in its acquisition costs.
 *
 * KEY AGENTS:
 *   - Credentialed incumbent practitioners: established professionals who benefit from supply restriction and wage floors. High power, arbitrage-level exit (can navigate reciprocity, credentialing renewal, credential portability). Constitute licensing boards and standard-setting bodies.
 *   - Low-resource non-credentialed workers: laborers without capital or family professional networks. Powerless, trapped exit (credential acquisition is structurally unavailable; leaving the profession is the only exit). Bears extraction through categorical exclusion.
 *   - Marginalized labor market entrants: individuals from historically excluded racial/ethnic groups and geographies. Powerless, identity-locked exit (have internalized the credential barrier as personal inadequacy; exit would require abandoning professional aspiration or recognizing systemic exclusion). Bears both structural and internalized suppression.
 *   - Credential examining bodies: state licensing boards and professional examination councils. Institutional power, constrained exit (nominally independent but operationally captured by incumbent practitioners). Enforce the constraint; administer the tiering mechanism.
 *   - Legislative oversight authorities: state legislatures. Institutional power, analytical exit. Rarely intervene in professional credentialing despite equity complaints.
 *   - Excluded advocates: consumer protection advocates and equity advocates absent from credentialing deliberations. Would propose alternative competence verification or fee waivers if seated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.72).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Licensing as Tiered Market Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '3c6cadb0-61ef-4fd0-a8d4-204d40e25341').
narrative_ontology:cs_kernel_codification('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', formalized).
narrative_ontology:cs_authority_grounding('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', extraction).
narrative_ontology:cs_interpretation_layer_present('3c6cadb0-61ef-4fd0-a8d4-204d40e25341').
narrative_ontology:cs_reading_relation('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', licensing_statute_mandate__rent_seeking_suppression, influences).
narrative_ontology:cs_axiom('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', foundational, credential_acquisition_cost_neutral).
narrative_ontology:cs_axiom_status(credential_acquisition_cost_neutral, overridden).
narrative_ontology:cs_axiom_grounding('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', credential_acquisition_cost_neutral, empirically_contingent).
narrative_ontology:cs_axiom('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', secondary, class_sorting_outcome_emergent_not_intentional).
narrative_ontology:cs_axiom_status(class_sorting_outcome_emergent_not_intentional, holdable).
narrative_ontology:cs_axiom_grounding('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', class_sorting_outcome_emergent_not_intentional, empirically_contingent).
narrative_ontology:cs_reference_frame('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', consumer_protection_mandate_frame).
narrative_ontology:cs_drift_state('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', contemporary_equity_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c6cadb0-61ef-4fd0-a8d4-204d40e25341', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_resource_non_credentialed_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_labor_market_entrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licensed practitioners in established fields (law, medicine, electrician trades, etc.) benefit from statutory credential requirements that restrict labor supply and maintain wage floors. They influence licensing board composition, examination standards, and reciprocity rules. Their credentials provide both genuine competence signal and market power against new entrants. They have resources to navigate examination costs, continuing education, and credential portability.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners, agenda_setter).

% Workers without capital, family professional networks, or stable housing bear the highest barrier cost to credential acquisition: examination fees ($200–$3,000 per credential), exam preparation courses ($500–$5,000), supervised work hours unpaid or at poverty wages (2–7 years typical), and opportunity costs of foregone labor income. For them, licensing statutes create categorical exclusion, not a standard. Exit from the constraint would require either waiving the credential requirement or acquiring it—both are structurally unavailable.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_resource_non_credentialed_workers, payer,
    powerless, biographical, trapped, national).

% Individuals from racial/ethnic groups, geographic regions, or socioeconomic backgrounds historically excluded from professional pathways face compounded barriers: they lack intergenerational credential familiarity, mentor networks, and sometimes remedial education access required to pass standardized exams (themselves culturally embedded). They internalize the credentialing narrative ('licensing protects consumers') while experiencing it as a gate that was never opened to their family. Exit would require abandoning either the profession or internalizing the barrier as personal inadequacy.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_labor_market_entrants, payer,
    powerless, biographical, identity_locked, national).

% State licensing boards, professional examination councils, and accreditation bodies set and enforce credential standards, examination difficulty, fee structures, and reciprocity agreements. They are nominally independent but operationally captured by incumbent practitioners who dominate board membership, examination committees, and standard-setting bodies. They defend credential requirements as public safety measures while resisting equity-focused reforms (fee waivers, exam language accommodation, alternative pathways).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credential_examining_bodies, agenda_setter,
    institutional, generational, constrained, national).

% State legislatures delegate credentialing authority to professional boards but retain nominal oversight. In practice they seldom intervene; professional boards are treated as technical bodies beyond electoral politics. Legislators occasionally hear complaints from consumer advocates and equity advocates, but credentialed incumbents have greater access and resources for persuasion.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, legislative_oversight_authorities, observer,
    institutional, generational, analytical, national).

% Advocates for consumer safety could argue for competence verification, but they are largely absent from credentialing debates. Instead, consumer protection and supply restriction have been fused as a single narrative; questioning the scope of licensing (by design) appears to question consumer safety itself. They would argue for narrower, outcome-based standards if seated at the table.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumer_protection_advocates, excluded,
    moderate, biographical, constrained, national).

% Labor equity and racial justice advocates recognize credentialing barriers as a mechanism of durable class and racial exclusion. They lack structural power to reshape credentialing regimes; when they propose alternatives (fee waivers, portfolio review, apprenticeship pathways), incumbent practitioners and examining bodies frame them as threats to consumer safety, and the equity proposal stalls.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, equity_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform competence standard for service-providers in regulated professions, reducing consumer search costs and protecting against fraud or incompetence that would require post-hoc litigation or regulatory investigation.
% TRANSFER_FUNCTION: Transfers labor market access from non-credentialed workers to credentialed workers by narrowing the supply of legal service-providers; also transfers rents (elevated wages) from consumers and from excluded workers to credentialed practitioners.
% ABSENT_VOICES: Workers unable to afford credential acquisition are structurally excluded—they have no seat at the licensing board and no legal standing to challenge credential standards. Equity advocates and consumer protection advocates (who might argue for narrower licensing or outcome-based standards) are absent from board deliberations and standard-setting. Workers from marginalized backgrounds who internalize the barrier as personal inadequacy carry the constraint as internalized suppression.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished, market competition on competence would restructure professional services: some incumbents would compete on price and would lose market share; new entrants from resource-constrained backgrounds would enter professions; consumer protection would shift from supply-restriction to reputation systems, insurance, and complaint mechanisms. Wage structures would flatten; rent collection would cease; the profession would open at the margin.
% FOUNDING_PROBLEM: Early professional practice in regulated fields (law, medicine, electrical work) involved fraud, incompetence, and consumer harm, with no mechanism for consumers to verify competence before purchasing services. Licensing statutes were justified as creating a uniform competence floor.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed practitioners and examining bodies attest the founding problem remains live and justify credentialing as ongoing consumer protection. Economists and equity advocates (from outside the benefiting parties) attest the founding problem is substantially solved—reputation systems, insurance, and professional liability address most fraud; licensing now functions primarily as supply restriction and rent extraction. Legislative testimony and published equity research support the contested reading. Consumer harm data for licensed professions are not systematically worse than for less-regulated professions at comparable price points.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72 at interval end) is high because the constraint strips labor market access from non-credentialed workers through barriers those workers cannot overcome, benefiting credentialed practitioners through both supply restriction (wage floors) and rents (elevated service prices). It is not extraction of a surplus the payer could theoretically offset—it is categorical exclusion enforced by law. Suppression (0.68) is substantial because the constraint operates partly through internalized barriers ('I am not qualified for this profession') maintained by the very credentialing narrative that justifies the barrier. Theater (0.44 at interval end, rising from 0.28) indicates growing defensive infrastructure: as equity challenges to credentialing intensify, credentialing bodies invest more in DEFENDING the consumer-protection narrative (regulatory capture theater) rather than expanding its actual function. The measurement series show extraction accumulating over the interval: extractiveness rises from 0.55 to 0.72 as credentialing scope expands (more professions licensed, reciprocity rules tighten) and as resource barriers harden (examination fees rise, apprenticeship duration extends). Theater rises as well—the defensive narrative intensifies. Suppression requirement rises from 0.52 to 0.68 because resistance from equity advocates increases, requiring more elaborate justification from examining bodies (consolidation of the 'consumer protection' framing, stricter enforcement against alternative credentialing pathways). The trajectories converge post-interval-30, suggesting the constraint has stabilized at its current extraction capacity: further gains would require raising barriers even higher, which would trigger legislative attention.
 *
 * PERSPECTIVAL GAP:
 *   The constraint computes very differently from the incumbent-practitioner seat versus the low-resource-worker seat. From the incumbent seat: this is a legitimate coordination mechanism protecting consumers, which the practitioner maintains through professional engagement and examination rigor. Extraction is framed as (incidental wage benefit from genuine competence signal). From the low-resource seat: this is categorical exclusion, a legal bar to livelihood, maintained by practitioners who benefit from supply restriction. The same enforcement activity (examination boards rejecting candidates, reciprocity rules restricting portability) appears as 'quality control' from one seat and as 'gatekeeping' from the other. The engine's per-seat computation captures this: the agenda-setter seat (examining bodies, credentialed practitioners) will compute this as a weaker type (closer to rope, genuine coordination) while the payer seat (low-resource workers) will compute this as snare (pure extraction with coordination cover). This divergence IS the story the constraint tells.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed incumbents have directionality near 0.0 (full beneficiary): they collect rents through wage floors and supply restriction, set the standards that exclude competitors, and have high-resource exit options (can migrate between jurisdictions with reciprocal credentialing, can retrain, can access capital). Low-resource workers have directionality near 1.0 (full target): they bear extraction through categorical exclusion, have no voice in standard-setting, have trapped or identity-locked exit (credential acquisition is materially unavailable; leaving the profession is surrender). Marginalized entrants are similar—powerless and identity-locked—but with the added internalization component: they have been socialized into accepting the barrier as legitimate, as personal responsibility rather than systemic exclusion. This internalization amplifies effective suppression beyond the structural barrier. Examining bodies sit nearer the beneficiary end (they collect prestige and institutional power from standard-setting) but are constrained by professional board membership and career dependence on incumbent populations. Legislative oversight authorities are analytical: they have nominal power but no structural incentive to intervene, and incumbent practitioners have greater lobbying access.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows strong mandatrophy signals: the founding problem ('consumer fraud and incompetence in unregulated practice') is contested—credentialed practitioners say it is live, economists and equity advocates say it is substantially solved by reputation systems and insurance. The disappearance_verdict is 'world_rearranges,' not 'world_unchanged,' indicating the constraint is not a natural fact but a sustained arrangement whose removal would reshape markets. Yet the credentialed incumbents and examining bodies continue to justify the constraint through the founding problem, which many observers outside the benefiting parties no longer accept as active. This is the classical mandatrophy signature: the mandate (consumer protection) has atrophied, but the constraint persists through defensive theater (credentialing boards elaborating justifications) and incumbent capture (practitioners retaining board power). The theater ratio rising from 0.28 to 0.44 over the interval exemplifies this: as the founding problem becomes more contested, defending the constraint requires more DEFENSIVE narrative, not more actual consumer-protection activity. The constraint is not decaying toward elimination—instead, its justification is theatricalized while its extraction function continues and hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalization_vs_structural,
    'Is the measured suppression (0.68) primarily structural (material barriers preventing credential acquisition) or internalized (belief systems and identity fusion that persist after barrier removal)?',
    'Natural experiments from jurisdictions that remove credential barriers (fee waivers, portfolio alternatives, apprenticeship pathways): if suppression drops substantially, it was primarily structural; if suppression persists or only partially declines, internalization is a large component. Longitudinal tracking of workers who begin pathways with and without identity-narrative intervention would test the internalization hypothesis.',
    'If suppression is primarily structural, policy solutions focus on barrier reduction (fee waivers, alternative pathways, loan forgiveness). If it is substantially internalized, remedies require deeper institutional change: decolonizing credentialing narratives, mentorship, visibility of successful practitioners from marginalized backgrounds, explicit system critique. The classification would not change, but effective suppression would drop if internalization were addressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization_vs_structural, empirical, 'Mechanism of suppression: structural barriers vs. internalized belief systems').

omega_variable(
    founding_problem_alive_vs_narrative_residue,
    'Is consumer fraud and incompetence in unregulated service provision genuinely a live problem at the current time (justifying continued credentialing scope), or is it a narrative residue from an earlier era when consumer protection mechanisms (reputation, insurance, liability) were not yet developed?',
    'Comparative data on consumer harm rates between licensed and unlicensed professions at similar price points; complaint and litigation rates; insurance claim frequencies. Analysis of whether consumer protection mechanisms (online reviews, professional insurance, liability suits, regulatory investigation post-harm) now adequately address the problem that credentialing was originally built to solve.',
    'If the founding problem is live, credentialing scope is justified and extraction is incidental to coordination. If it is narrative residue, credentialing scope should be narrowed and extraction becomes the primary function. The classification would shift from snare (if founding problem were live) toward pure extraction; in the current reading it remains snare because the problem is contested (some credentialing bodies maintain it is live; external observers hold it is substantially solved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_alive_vs_narrative_residue, empirical, 'Whether the founding problem (consumer harm from incompetence) remains a live constraint on professional practice or has been substantially addressed by market and insurance mechanisms').

omega_variable(
    credentialing_necessity_vs_supply_restriction_cover,
    'Are the credential-acquisition barriers (fees, apprenticeship duration, examination difficulty, reciprocity restrictions) scaled to match the complexity and risk of the profession, or are they set at a level above what is minimally necessary to verify competence, functioning as supply restriction disguised as quality assurance?',
    'Benchmarking across jurisdictions with varying barrier levels: do jurisdictions with lower barriers show higher consumer harm rates or lower professional quality? International comparison with professions that use portfolio-based verification, shorter apprenticeships, or lower-cost examination. Regulatory analysis of whether board-approved barriers have been increasing over time while the underlying risk profile of practice has remained stable (barrier creep).',
    'If barriers are necessarily scaled to risk, they are justified as coordination cost. If they exceed necessity and have been creeping upward, the barriers are supply-restriction mechanisms. This would support the rent_seeking_suppression reading over the public_safety_coordination reading, and would strengthen the mandatrophy analysis: the founding problem is solved, but barriers persist and intensify, driving extraction growth while the justification theatricalizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_necessity_vs_supply_restriction_cover, empirical, 'Whether credential-acquisition barriers are scaled to competence verification necessity or function as supply restriction in excess of necessity').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Do the three readings of the licensing_statute_mandate kernel (graduated_access_filter, public_safety_coordination, rent_seeking_suppression) coexist as live positions or does one foreclose another''s core premise within a single policy framework?',
    'Institutional analysis: do licensing boards, practitioners, and legislators hold multiple readings simultaneously, or does commitment to one reading logically exclude commitment to another? Can a licensing board sincerely defend ''this system protects consumers'' (public_safety) while also acknowledging ''this system restricts labor supply for incumbent benefit'' (rent_seeking)? Or do these premises contradict?',
    'If readings coexist (different stakeholders hold different readings without logical contradiction), they are siblings with coexists_with relation. If one reading''s core claim directly contradicts another''s core claim (e.g., ''the consumer-protection function is live'' vs. ''the consumer-protection function is cover for supply restriction''), then foreclosure applies. This affects how institutional change proceeds: coexisting readings can be negotiated and accommodated; foreclosing readings require one side to be displaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'Logical relationship between sibling readings: do they coexist or does one foreclose another?').

omega_variable(
    identity_locked_vs_trapped_exit_divergence,
    'For marginalized labor-market entrants coded as identity_locked, would removing structural barriers (credential fees, apprenticeship duration) substantially increase professional entry, or would internalized identity barriers prevent entry even after structural barriers fell?',
    'Prospective: natural experiments removing barriers while measuring take-up among marginalized groups; testing whether identity-narrative interventions (mentorship, visibility, explicit system critique) increase entry beyond what barrier-removal alone achieves. Retrospective: cases where barriers were substantially reduced (through policy intervention or economic expansion allowing more people to accumulate capital)—did marginalized entrants flood into the profession, or did entry remain low despite barrier reduction?',
    'If removing barriers substantially increases entry without identity-narrative work, identity_locked classification was incorrect; exit is actually constrained (material), not identity-locked (psychological). If entry remains low even after barriers fall, identity-locking is confirmed—the classification is correct, and deeper interventions are needed. This affects remediation strategy: barrier-removal alone may be insufficient if identity-locking is the primary suppression mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_exit_divergence, empirical, 'Whether marginalized entrants'' non-entry into credentialed professions is driven by structural barriers (trapped exit) or by internalized belief systems (identity-locked exit)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lice_tr_t5, licensing_statute_mandate__graduated_access_filter, theater_ratio, 5, 0.32).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.36).
narrative_ontology:measurement(lice_tr_t15, licensing_statute_mandate__graduated_access_filter, theater_ratio, 15, 0.39).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.41).
narrative_ontology:measurement(lice_tr_t25, licensing_statute_mandate__graduated_access_filter, theater_ratio, 25, 0.43).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.44).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lice_be_t5, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(lice_be_t15, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(lice_be_t25, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(lice_su_t5, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(lice_su_t15, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(lice_su_t25, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, resource_allocation).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__graduated_access_filter, 0.18).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'licensing_statute_mandate.' The kernel itself (statutory credential requirements) is shared with two sibling constraints: 'public_safety_coordination' frames credentialing as consumer protection; 'rent_seeking_suppression' frames it as cartel behavior. All three readings have the SAME REFERENT (the statutory credential requirement itself) but different ε values because they measure different aspects of the constraint's function. The 'graduated_access_filter' reading has higher extractiveness (0.72) because it focuses on the material class-sorting effect; 'public_safety_coordination' would have lower extractiveness (because coordination cost is the primary measurement) but possibly higher theater (if the coordination function is largely symbolic); 'rent_seeking_suppression' would have similar extractiveness to this reading but would foreground the supply-restriction intent rather than class-sorting pattern. Each reading is a complete, ε-invariant constraint story; the three readings together form a constraint FAMILY documenting how different parties read the same kernel differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__graduated_access_filter, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
