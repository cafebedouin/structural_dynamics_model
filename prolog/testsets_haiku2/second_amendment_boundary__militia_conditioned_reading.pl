% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Reading: State Regulatory Authority over Firearms
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The Second Amendment text reads: 'A well regulated Militia, being
 *   necessary to the security of a free State, the right of the people to
 *   keep and bear Arms, shall not be infringed.' The militia-conditioned
 *   reading interprets the prefatory clause ('A well regulated Militia, being
 *   necessary to the security of a free State') as scoping and limiting the
 *   operative right ('the right of the people to keep and bear Arms'). Under
 *   this reading, the right is collective and conditioned: it exists to
 *   support a well-regulated militia in collective defense, not to ground
 *   individual gun ownership independent of militia service. This reading
 *   presumptively legitimates state legislative authority to regulate private
 *   firearms possession through public-safety justifications. Gun owners
 *   whose possession claims rest on hobby, collecting, or individual
 *   self-defense face the burden of proving their possession serves the
 *   militia purpose — a burden they cannot meet under the reading's logic,
 *   making comprehensive regulation constitutional. The constraint is CLAIMED
 *   as tangled_rope (real coordination function + asymmetric extraction +
 *   active enforcement) and authored metrics consistently describe extractive
 *   operation: legislative authority gains decisional space, gun owners lose
 *   it, and active suppression (litigation burden, regulatory scrutiny,
 *   cultural delegitimation) holds the reading in place despite sustained
 *   resistance from gun rights advocates.
 *
 * KEY AGENTS:
 *   - Democratic legislative bodies — institutional agenda-setters who gain authority to regulate; the reading legitimates their restrictions
 *   - Public safety institutions (law enforcement, public health bodies) — beneficiaries who gain operational authority and enforcement prerogatives
 *   - Unrestricted private gun owners (moderate power, constrained exit) — payers who bear licensing, restrictions, and lost market access
 *   - Firearms collectors (moderate power, identity-locked exit) — payers whose hobby identity is delegitimated by the reading's scope limitation
 *   - Self-defense claimants in high-regulation jurisdictions (powerless, trapped exit) — payers who cannot exit or overcome the reading's presumption against their claim
 *   - Individual-right advocates (organized, constrained exit) — excluded from the reading's legitimating framework; their core premise is what the reading targets for displacement
 *   - Supreme Court majority (institutional power, analytical distance) — observers whose rulings determine which reading holds institutional authority (currently favoring individual-right reading)
 *   - State legislatures in high-regulation jurisdictions (institutional beneficiaries) — gain policy space and authority validation from the militia-conditioned reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.71).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Reading: State Regulatory Authority over Firearms").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, 'ae2c0660-138d-4f01-b877-4d28c82003c6').
narrative_ontology:cs_kernel_codification('ae2c0660-138d-4f01-b877-4d28c82003c6', fixed_text).
narrative_ontology:cs_authority_grounding('ae2c0660-138d-4f01-b877-4d28c82003c6', lineage).
narrative_ontology:cs_interpretation_layer_present('ae2c0660-138d-4f01-b877-4d28c82003c6').
narrative_ontology:cs_reading_relation('ae2c0660-138d-4f01-b877-4d28c82003c6', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ae2c0660-138d-4f01-b877-4d28c82003c6', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('ae2c0660-138d-4f01-b877-4d28c82003c6', foundational, prefatory_clause_scopes_operative_right).
narrative_ontology:cs_axiom_status(prefatory_clause_scopes_operative_right, overridden).
narrative_ontology:cs_axiom_grounding('ae2c0660-138d-4f01-b877-4d28c82003c6', prefatory_clause_scopes_operative_right, empirically_contingent).
narrative_ontology:cs_axiom('ae2c0660-138d-4f01-b877-4d28c82003c6', foundational, militia_service_required_for_right_invocation).
narrative_ontology:cs_axiom_status(militia_service_required_for_right_invocation, overridden).
narrative_ontology:cs_axiom_grounding('ae2c0660-138d-4f01-b877-4d28c82003c6', militia_service_required_for_right_invocation, conventional).
narrative_ontology:cs_reference_frame('ae2c0660-138d-4f01-b877-4d28c82003c6', militia_centered_regulatory_authority).
narrative_ontology:cs_drift_state('ae2c0660-138d-4f01-b877-4d28c82003c6', contemporary_post_bruen_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ae2c0660-138d-4f01-b877-4d28c82003c6', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, democratic_legislative_bodies).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_institutions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, unrestricted_private_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures_in_high_regulation_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce firearms regulations; the militia-conditioned reading legitimates their authority by interpreting the prefatory clause as granting state regulatory power. They benefit from expanded policy space and constitutional presumption that their regulations are valid unless proven irrational. They do not bear the costs of restriction — gun owners do.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, democratic_legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Law enforcement, public health authorities, and regulatory bodies gain enforcement authority and presumed legitimacy. They screen gun ownership, conduct background checks, and implement restrictions. They collect authority and operational capacity without bearing regulation costs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_institutions, beneficiary,
    institutional, generational, analytical, national).

% Bear the full cost of regulation: licensing requirements, background checks, waiting periods, weapons-class restrictions, and outright prohibitions. The reading presumptively invalidates their claims unless they can prove militia service, shifting burden of justification onto them. Exit is constrained because the restriction is national (no refuge jurisdiction) and because gun ownership is often identity-central.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, unrestricted_private_gun_owners, payer,
    moderate, biographical, constrained, national).

% Collect firearms for historical, technical, or aesthetic reasons. The militia-conditioned reading excludes hobby and historical interest as legitimate bases for possession — only militia service counts. Many collectors identify strongly with their hobby and community; exit means abandoning that identity. They face restrictions on acquisition, ownership, and transfer of collection items.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, identity_locked, national).

% Seek guns for personal protection in jurisdictions with severe restrictions or bans. The reading does not recognize self-defense as a grounding for the right — militia service is the only legitimate frame — so their claims meet the presumption that regulation is constitutional. They are trapped because: (1) exit requires relocation (high cost, geographic constraint), (2) self-protection is identity-fused (they understand their safety as contingent on their capacity to defend themselves), and (3) they have minimal political power to overturn the reading institutionally. They face the highest practical extraction and the lowest resistance capacity.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_jurisdictions, payer,
    powerless, biographical, trapped, local).

% Constitutional scholars, advocacy organizations (NRA legal division, Gun Owners of America, Second Amendment scholars), and litigants who argue the operative clause establishes an individual right independent of militia service. They are structurally excluded from the militia-conditioned reading's legitimating framework — their core interpretive premise is the reading's target. They would object to the reading if present in the constitutional-interpretation room, but the reading operates to keep their interpretation out of official doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, biographical, constrained, national).

% The sitting Supreme Court (as of 2022-2026) has rejected the militia-conditioned reading and adopted the individual-right reading (District of Columbia v. Heller 2008, New York State Rifle & Pistol Association v. Bruen 2022). They serve as the authoritative interpreter of the Constitution and thus determine which reading holds institutional power. The militia-conditioned reading survives in dissent and in some state legislatures, but has lost its primary source of legitimating authority.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, supreme_court_majority, observer,
    institutional, generational, analytical, national).

% States like California, New York, Massachusetts, and Illinois that have enacted strict gun regulations benefit from the militia-conditioned reading because it constitutionalizes their authority to restrict. The reading transforms regulations from constitutionally suspect (requiring strict scrutiny justification) into presumptively valid (rational basis only). They collect policy space and authority.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures_in_high_regulation_jurisdictions, beneficiary,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, democratic_legislative_bodies).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the republic's collective-defense capacity by establishing state authority to regulate private arms possession in service of a well-regulated militia — solving the collective-action problem of how to maintain militia readiness without allowing private accumulation of arms to destabilize democratic governance.
% TRANSFER_FUNCTION: Transfers constitutional authority from individual gun owners (who claim pre-political rights to possess) to legislatures and public-safety institutions (who claim authority to regulate). Also transfers regulatory costs from legislatures to gun owners: the cost of proving militia service, the cost of licensing and background checks, the cost of restricted market access.
% ABSENT_VOICES: Individual-right advocates and constitutional scholars who read the operative clause as establishing an individual right independent of militia service. These voices would argue the militia-conditioned reading misreads the constitutional text, subordinates individual liberty to regulatory convenience, and improperly elevates the prefatory clause above the operative clause. They are structurally excluded from the reading's legitimating framework.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned reading disappeared and the individual-right reading took exclusive institutional authority, firearms regulations in high-restriction jurisdictions would face heightened scrutiny and many would be struck down. Gun ownership would expand, the private firearms market would grow, and gun-rights advocacy would shift from fighting for constitutional standing (which they now have post-Bruen) to fighting against specific regulations. Conversely, if the militia-conditioned reading were institutionalized as the supreme interpretation, gun ownership would be further restricted, the market would contract, and gun owners would lose the litigation standing they currently possess.
% FOUNDING_PROBLEM: Prevent armed private accumulation from destabilizing the republic while maintaining militia readiness for national defense. The founders were concerned about standing armies as threats to liberty, so they constitutionalized the militia as a check. But they also needed to prevent private citizens from accumulating weapons that could overthrow the government or create private military power. The militia clause frames the right as solving this coordination problem: citizens can keep arms, but only as part of a state-regulated militia system, not as private individuals.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is understood by legal historians (Garry Wills, Michael Waldman) as a response to 18th-century concerns about standing armies. That problem has been substantially resolved by modern political evolution (standing armies are now the standard for national defense; militias are vestigial). The militia-conditioned reading persists partly through institutional inertia and partly through theoretical performance — it invokes a militia framing that has no actual role in modern defense systems. The Supreme Court's rejection of the reading (Bruen 2022) reflects a judicial determination that the founding problem is no longer live and the militia clause does not scope-limit the operative right. Gun-rights scholars (Randy Barnett, Eugene Volokh) concur that the founding concern has evolved and the militia-centered reading is anachronistic. Dissenting voices from public-health and safety institutions (American Medical Association, American Psychological Association) argue that even if the founding problem is technically dead, the updated problem of gun violence justifies regulation — but they ground this in policy arguments about contemporary harms, not in the militia-conditioned reading's original logic.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.42 to 0.68 over the interval, plateauing after point 32, reflecting the reading's gradual entrenchment through litigation and legislative action (2008-2022), then stabilizing as the Supreme Court moved away from the militia-conditioned reading and toward individual-right authority (Heller 2008, Bruen 2022). The reading's extractiveness is high because it imposes asymmetric costs on gun owners whose claims it delegitimizes, while benefiting legislatures and regulatory bodies. Suppression requirement is also high and stable (0.55→0.71) because the reading's persistence depends on active exclusion of individual-right interpretations from judicial and legislative authority — the suppression is interpretive (keeping alternative readings out of the legitimating framework), not merely physical. Theater ratio is moderate (0.25→0.42) because the reading does invoke a real founding purpose (militia readiness, collective defense) but increasingly performs that purpose rather than fulfilling it (no modern militia system actually depends on the reading's regulatory framework, so the constraint's functional role has atrophied). The plateau in metrics after point 32 reflects the Supreme Court's decisive move toward individual-right authority (Bruen 2022), which reduced the reading's institutional force — the reading persists in dissent and in some state legislatures, but no longer controls the official interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The legislative-body and regulatory seats should compute as moderate beneficiaries (d ~ 0.25-0.35): the reading gives them authority, they bear minimal restriction costs, they collect policy space. Gun-owner seats should compute as high targets (d ~ 0.65-0.85): they bear comprehensive restriction, constrained exit (trapped geographically, identity-locked culturally), they face litigation burden. The gap is extreme because one seat gains institutional legitimacy while the other loses standing. The reading produces this divergence by making the militia clause the scope-limiter: once adopted, it shifts where the burden of justification falls, which is why the same constitutional text produces radically different experienced constraints depending on which seat you occupy. The engine derives d from beneficiary/victim declarations + exit options; the commentary explains why the same constraint looks like coordination to beneficiaries and like extraction to payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (democratic legislatures, public-safety institutions) benefit from expanded regulatory authority at minimal personal cost. They declare the coordination problem (militia readiness) and solve it by restricting private possession — their extraction of authority is presented as solving that problem. Victims (gun owners across all categories) bear comprehensive costs: licensing burden, restrictions, delegitimation of their ownership claims, constrained exit. Exit is constrained because: (1) Geographically — no U.S. jurisdiction recognizes an individual right sufficiently broad to escape restriction; (2) Identity-locked for collectors and self-defense claimants — their self-concept and community ties are fused with gun ownership, so exit means identity abandonment. The powerless self-defense claimants in high-regulation jurisdictions face trapped exit: they cannot relocate (cost), cannot exit gun ownership (need), cannot organize politically (powerless). The reading systematically favors beneficiaries by presupposing the militia context as the only legitimate framing — any other framing (hobby, individual liberty, self-defense) is subordinated as non-constitutional.
 *
 * MANDATROPHY ANALYSIS:
 *   The militia-conditioned reading sits at the boundary between tangled_rope and potential piton status. It is tangled_rope because: (1) Real coordination function — militia readiness is a genuine collective-action problem the founders addressed; (2) Asymmetric extraction — legislatures gain authority, gun owners lose it, mediated through the same constraint; (3) Active enforcement required — the reading's persistence depends on excluding individual-right interpretations from institutional authority, which requires ongoing judicial and legislative work. However, the measurement series and the Supreme Court's institutional trajectory suggest potential mandatrophy: the founding problem (militia readiness as a check on standing armies) is no longer live in its original form (no modern militia system actually depends on the reading's scoping of the Second Amendment). The reading persists partly through institutional inertia (entrenched in some state legislatures) and increasingly through theatrical performance (invoking a militia frame that has no practical institutional role). The theater_ratio climb from 0.25 to 0.42 tracks this degradation. If the reading were fully mandatrophied, it would be a piton: still extracting (legislatures still collect authority), but no longer coordinating anything real, held in place by enforcement habit and institutional architecture rather than by solving the problem it claims to solve. The current state is contested mandatrophy: the Supreme Court has ruled against the militia-conditioned reading (Bruen 2022, holding the right is not limited to militia service), yet the reading persists in dissent and in some state jurisdictions, so the question of whether the founding problem is dead remains live in the judicial and legislative arena.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_scope_limiter_vs_prefatory_purpose,
    'Does the prefatory militia clause limit the scope of ''keep and bear Arms'' (making it conditional on militia service), or does it merely state a purpose while the operative clause establishes an unconditional individual right?',
    'Historical linguistic analysis of 18th-century constitutional preambles and similar conditional structures. Contemporary jurisprudence from the Supreme Court (which has largely rejected the militia-conditioned reading post-Heller 2008). Natural-language corpora of founding-era documents to test whether prefatory clauses in similar texts function as scope-limiters or as non-limiting preambles.',
    'If the militia clause is scope-limiting, the militia-conditioned reading is correct and gun owners'' claims rest on militia service — comprehensive regulation is constitutional. If the militia clause is non-limiting purpose-statement, the operative clause establishes an individual right and gun owners'' claims are grounded independently — regulations face heightened scrutiny. The Supreme Court''s institutional ruling (Heller 2008) has moved the jurisprudence toward scope-non-limiting, making the militia-conditioned reading''s structural premise false in official doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_scope_limiter_vs_prefatory_purpose, empirical, 'The grammatical and historical question of whether the prefatory clause limits scope or merely states purpose.').

omega_variable(
    militia_service_definition_boundary,
    'What counts as ''a well regulated Militia'' for purposes of the Second Amendment right? Does it include: (a) formal state National Guard units, (b) informal local militia organizations, (c) any armed citizenry capable of collective defense, (d) the unorganized militia (all able-bodied citizens), or (e) some subset?',
    'State militia statutes and federal law definitions (10 U.S.C. § 246). Historical reconstruction of the founders'' understanding of militia (e.g., Federalist Papers, state constitutions). Jurisprudential evolution in cases addressing what militia service means (Presser v. Illinois 1886, U.S. v. Miller 1939, DC v. Heller 2008).',
    'If militia = formal National Guard only, the reading''s scope is narrow and private gun ownership is highly restricted. If militia = any organized armed group, the scope broadens and more ownership claims qualify. If militia = unorganized militia (all citizens), the reading converges toward the individual-right reading. The definition determines how many gun owners fall under the militia-service exception and how restrictive the regulation can be.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_service_definition_boundary, empirical, 'The definitional boundary of what counts as militia service for Second Amendment purposes.').

omega_variable(
    constitutional_reading_as_extracted_authority_vs_discovered_truth,
    'Is the militia-conditioned reading a true interpretation of the constitutional text, or is it an exercise of interpretive authority that extracts legitimacy for state regulation by asserting a particular reading?',
    'Meta-constitutional analysis: if the text supports multiple coherent readings (as originalist and living-constitution scholars dispute), then constitutional readings are in some sense constructed from available textual materials rather than discovered as pre-existing truths. The fact that the Supreme Court has institutionally rejected the militia-conditioned reading (Bruen 2022) suggests the ''truth'' of the reading is not independent of who has authority to declare it.',
    'If readings are discovered truths, the militia-conditioned reading stands or falls on historical and linguistic evidence. If readings are interpretive exercises, the reading''s persistence depends on the institutional seats that affirm it — when the Supreme Court rejects it, the reading loses its primary legitimating power even if historical evidence doesn''t change. This omega highlights the committer-frame paradox: a reading is authored as the truth the beneficiaries believe, not as the truth an external observer would discover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_reading_as_extracted_authority_vs_discovered_truth, conceptual, 'Whether constitutional readings are discovered interpretations or constructed exercises of interpretive authority.').

omega_variable(
    suppression_mechanism_state_violence_vs_interpretive_exclusion,
    'Is the suppression measured (0.71 at interval end) state physical coercion (arresting gun owners, confiscating weapons) or interpretive/institutional suppression (excluding alternative readings from official doctrine, shifting litigation burden)?',
    'Empirical tracking of enforcement mechanisms: what proportion of suppression operates through direct legal coercion (arrests, confiscation) vs. through interpretive authority (judicial rulings that make gun-owner claims non-justiciable, shifting burden of proof, delegitimating certain ownership categories). Interviews and survey data from gun owners about whether they experience the constraint as physical coercion or as loss of interpretive standing.',
    'If suppression is primarily physical coercion, gun owners are actively resisting and the constraint is high-friction. If suppression is primarily interpretive (they have been made to believe their claims are constitutionally illegitimate), the constraint is more internalized and more stable. Internalized suppression is harder to overturn because it requires not just policy change but interpretive reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_state_violence_vs_interpretive_exclusion, empirical, 'Whether suppression operates through state violence or through interpretive/institutional authority.').

omega_variable(
    founding_problem_militia_readiness_vs_tyranny_prevention,
    'What problem did the Second Amendment actually address: (a) ensuring a well-regulated militia could be mobilized for national defense and internal order, or (b) preserving private armed capacity as a check against tyrannical government?',
    'Historical documentary evidence from the Founding era: Constitutional Convention records, Federalist Papers, state ratification debates, militia laws of the founding states. Modern historiography (Garry Wills, Michael Waldman vs. Randy Barnett, Eugene Volokh). The fact that the founders'' concern about standing armies is now moot (professional standing armies are the standard, no modern republic relies on citizen militias for defense) supports a reading that the founding problem is dead, not live.',
    'If the problem is militia readiness, the militia-conditioned reading''s functional purpose is obsolete (no modern militia system depends on private gun ownership). If the problem is tyranny-prevention, the insurrectionist reading''s logic becomes stronger. The founding_problem_status determination (live vs. dead vs. contested) depends on this resolution. If dead, mandatrophy diagnosis becomes relevant: the constraint persists without solving the problem it claims to solve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_militia_readiness_vs_tyranny_prevention, empirical, 'Whether the Second Amendment''s founding problem was militia readiness or tyranny prevention, and whether that problem remains live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(seco_tr_t16, observed).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(seco_tr_t24, observed).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(seco_tr_t32, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(seco_be_t16, observed).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(seco_be_t24, observed).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(seco_be_t32, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(seco_su_t16, observed).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(seco_su_t24, observed).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(seco_su_t32, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment boundary kernel has three distinct constraint instantiations, one per reading. The militia-conditioned reading interprets the prefatory clause as scope-limiting, establishing state regulatory authority presumed legitimate. The individual-right reading interprets the operative clause as independent and individual, shifting burden of justification onto regulators. The insurrectionist reading frames the right as an anti-tyranny reserve, making both state regulation and disarmament illegitimate. These are three different constraints from one kernel because their ε values, beneficiary/victim structures, and types differ: militia-conditioned is tangled_rope (high extraction), individual-right is rope or constrained-snare (lower extraction), insurrectionist is snare (extraction + high suppression + victim narrative). They are linked by network.affects_constraints because institutional adoption of one reading directly affects the others' plausibility and legal standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
