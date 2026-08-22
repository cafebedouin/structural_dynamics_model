% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Prohibition on Nonconsensual Medical Intervention (Bodily Autonomy Primary Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel; the kernel and
 *   its siblings are named in kernel_context and routed through omega
 *   variables, not described as contest inside this constraint. The reading,
 *   bodily_autonomy_primary, holds the coercion-legitimacy boundary as a
 *   categorical side-constraint: medical intervention without consent is
 *   impermissible regardless of collective benefit, and the arrangement under
 *   contest is the standing constitutional settlement enforcing that rule —
 *   mandate statutes struck down as a class, without balancing. The epsilon
 *   referent is that standing arrangement as it actually operates, assessed
 *   by the reading's own lights; it is never the mandate regime the sibling
 *   readings would install. Structurally the arrangement has a genuine
 *   coordination function (a verification-cheap bright line that protects
 *   every body from collective override and preserves the trust conditions of
 *   voluntary medicine), an identifiable cost-bearing set (immunocompromised
 *   individuals, who pay in exposure risk for a guarantee they cannot use),
 *   and active enforcement (courts against recurrent mandate legislation) —
 *   the tangled_rope signature. The categorical rhetoric is mountain-shaped;
 *   the structure is constructed, enforced, and contested, and the metrics
 *   are authored independently of that rhetoric.
 *
 * KEY AGENTS:
 *   - constitutional_courts: Agenda-setter (institutional/constrained) — administers and enforces the categorical boundary via judicial review; could revisit it only by overruling settled doctrine
 *   - bodily_autonomy_claimants: Primary beneficiary (moderate/mobile) — every person holding an absolute veto over intervention on their body
 *   - vaccine_decliners: Concrete beneficiary (moderate/mobile) — the shield's heaviest users; refusal carries no sanction under any disease condition
 *   - civil_liberties_organizations: Beneficiary (organized/identity_locked) — litigate the boundary; docket, funding, and mission are constituted by its persistence
 *   - public_health_authorities: Qualified beneficiary and payer (institutional/constrained) — barred from compelling intervention; gain trust-preservation and liability shelter, lose the strongest outbreak-control tool
 *   - immunocompromised_individuals: Primary payer (powerless/trapped) — depend on herd immunity the categorical rule declines to compel; pay in exposure risk
 *   - future_outbreak_victims: Excluded (powerless/trapped) — cannot yet object; enter the record only as retrospective statistics
 *   - medical_ethicists: Analytical observer (analytical/analytical) — map where the categorical rule and balancing frameworks come apart; collect nothing, bear nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Prohibition on Nonconsensual Medical Intervention (Bodily Autonomy Primary Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '92d018cf-1eb4-4621-b4c6-345b200a8b16').
narrative_ontology:cs_kernel_codification('92d018cf-1eb4-4621-b4c6-345b200a8b16', formalized).
narrative_ontology:cs_authority_grounding('92d018cf-1eb4-4621-b4c6-345b200a8b16', lineage).
narrative_ontology:cs_interpretation_layer_present('92d018cf-1eb4-4621-b4c6-345b200a8b16').
narrative_ontology:cs_reading_relation('92d018cf-1eb4-4621-b4c6-345b200a8b16', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('92d018cf-1eb4-4621-b4c6-345b200a8b16', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('92d018cf-1eb4-4621-b4c6-345b200a8b16', foundational, nonconsensual_medical_intervention_categorically_impermissible).
narrative_ontology:cs_axiom_status(nonconsensual_medical_intervention_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('92d018cf-1eb4-4621-b4c6-345b200a8b16', nonconsensual_medical_intervention_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('92d018cf-1eb4-4621-b4c6-345b200a8b16', foundational, collective_benefit_never_overrides_bodily_integrity).
narrative_ontology:cs_axiom_status(collective_benefit_never_overrides_bodily_integrity, holdable).
narrative_ontology:cs_axiom_grounding('92d018cf-1eb4-4621-b4c6-345b200a8b16', collective_benefit_never_overrides_bodily_integrity, deontological).
narrative_ontology:cs_reference_frame('92d018cf-1eb4-4621-b4c6-345b200a8b16', inviolable_bodily_integrity_side_constraint).
narrative_ontology:cs_drift_state('92d018cf-1eb4-4621-b4c6-345b200a8b16', contemporary_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92d018cf-1eb4-4621-b4c6-345b200a8b16', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_claimants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_decliners).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_organizations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_integrity_inviolability_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, nuremberg_informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review legislation and executive action against the categorical boundary and strike down mandate statutes that would compel medical intervention, without balancing. They define the scope of 'intervention,' 'consent,' and the carve-out classes (involuntary psychiatric treatment, child-welfare medical overrides, emergency powers). Their review is the mechanism that holds the boundary in place; revisiting it would require overruling their own settled doctrine at recurring legitimacy cost.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Every person whose body the rule shields from compelled intervention. They hold an absolute veto over what is done to their body, exercisable against majorities and the state alike; the protection follows them everywhere in the jurisdiction and cannot be suspended by emergency declaration. Most never invoke it; it operates as a standing guarantee whose value is highest precisely when panic is highest.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_claimants, beneficiary,
    moderate, biographical, mobile, national).

% The most concrete users of the shield: individuals who decline vaccination or other intervention and cannot be compelled under any disease condition. Their refusal carries no penalty, exclusion, or legal condition, and their own exposure risk is self-assumed. The rule converts what other frameworks treat as a sanctionable choice into an inviolable one.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_decliners, beneficiary,
    moderate, biographical, mobile, national).

% Litigate to maintain and extend the boundary. Their docket, membership, funding, and institutional mission are constituted by the rule's persistence: each mandate proposal generates standing, donors, and doctrinal territory. Their identity is fused with the principle — exit would dissolve the organization as such, not merely change its portfolio.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Run voluntary immunization, surveillance, and outbreak response under a rule that bars them from compelling intervention. They gain what they argue is the trust precondition of voluntary uptake, insulation from being conscripted as the coercive arm of political panic, and legal shelter from mandate liability. They simultaneously lose their strongest outbreak-control tool and bear the operational cost of containing epidemics without it — a dual position the role lists alone cannot express.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer).

% Depend on herd immunity because vaccination is contraindicated or ineffective for them. The categorical rule removes the mandate pathway that would suppress transmission around them; their protection now rests entirely on neighbors' voluntary choices. Their medical condition pins them to the exposure — isolation is the only exit and it is a shrinking social world. They are dispersed, weakly organized, and pay in infection risk for a guarantee they did not ask for and cannot use.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% People who will be infected in outbreaks that a mandate-capable regime might have truncated. They cannot yet object; they enter the record only retrospectively, as injury statistics cited by whichever side finds them useful. No seat at the table speaks for them while the boundary is being drawn and defended.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, future_outbreak_victims, excluded,
    powerless, generational, trapped, national).

% Analyze the boundary from outside its enforcement: map where the categorical rule and balancing frameworks come apart, audit the carve-outs, and testify in litigation. They collect no benefit and bear no cost of the arrangement; their stake is the coherence of the framework itself.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of mutual vulnerability to medical and state power with a verification-cheap bright line: no one's body can be invaded for others' benefit, so no one must litigate case-by-case whether their autonomy will be balanced away in the next emergency. The bright line also underwrites the trust conditions on which voluntary medicine and voluntary uptake depend.
% TRANSFER_FUNCTION: Moves residual disease risk onto those who cannot be protected by consent-based means — the immunocompromised and the not-yet-vaccinatable — so that everyone else holds an inviolate body. Moves decision authority over each body from the state and the majority to the individual. Moves legal certainty from case-by-case balancing to a fixed rule.
% ABSENT_VOICES: The future outbreak casualty — the person who will be infected because transmission was never suppressed by mandate — is structurally absent: they cannot yet object and appear only as retrospective statistics. The immunocompromised are present but weakly organized, and their strongest potential spokespeople are the mandate proponents the rule exists to bind.
% DISAPPEARANCE_RATIONALE: If the categorical boundary vanished overnight, mandate statutes would follow the next serious outbreak within months, the litigation practice built on the boundary would dissolve, the immunocompromised would gain a protection floor they currently lack, and the trust settlement between medicine and the decliner public would have to be renegotiated under coercion rather than consent. The arrangement's beneficiaries actively defend it; its disappearance would be contested, not absorbed.
% FOUNDING_PROBLEM: The historical record that produced the boundary: nonconsensual experimentation and sterilization carried out under collective-benefit rhetoric (the Nuremberg-era atrocities, Tuskegee, forced sterilization programs), and Jacobson-era compulsion, in which balancing frameworks systematically failed the marginal and the powerless. The categorical rule was built to end the balancing that had licensed those practices.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set by the Nuremberg tribunal record and the Belmont Report — authored by the biomedical and legal establishment that would otherwise be the rule's target class, not by autonomy claimants. What is contested is the status: mandate proponents attest the founding problem has inverted (the live danger is now under-protection of the vulnerable, not overreach), while the reading's adherents and the research-ethics tradition attest it remains live wherever collective-benefit rhetoric meets a defenseless body.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) because the arrangement runs no coercive machinery against bodies — its cost is concentrated residual risk and forgone protection, not compelled procedure — and it rises over the interval as epidemiological capacity grows and the opportunity cost of the categorical rule deepens. Suppression (0.45) is the enforcement face: courts must actively strike recurrent mandate legislation against outbreak-driven majorities, and the payer seat has few alternatives (isolation, PPE, passive prophylaxis). Theater is low (0.15): the boundary does real work — statutes fall, refusals stand — and the declaratory era's performative share (0.55 at interval start, when the rule was mostly aspiration in ethics codes) declined as the rule hardened into justiciable doctrine. Accessibility collapse is low-moderate (0.35): the balancing alternatives remain fully live in discourse, scholarship, and other jurisdictions; the rule blocks them domestically without collapsing them. Resistance is substantial (0.6): every serious outbreak regenerates mandate proposals, override attempts, and doctrinal challenge. All three tracked metrics share one nine-point grid; the rising suppression_requirement series is authored deliberately because the story specifically tracks enforcement-capacity build-out (soft ethics to constitutional review), not merely extraction drift. Receipt surface: the core gain — inviolate bodies — accrues to the universal class of claimants rather than to any named seat; the liberties bar and the courts collect secondary institutional rents (standing, funding, doctrinal territory) from the enforcement activity, but no seat captures the constraint's core gain, hence gain_flow is the affirmative 'diffuse'. Fixing cost is prohibitive: the boundary is entrenched doctrine, and reversal would require overruling a settled rights settlement against a mobilized liberties bar — a cost paid in legitimacy by the courts, not in risk by those who would benefit. That prohibitive cell reflects defended entrenchment, not administrator inertia: concentrated beneficiaries actively maintain the arrangement, which is the tangled_rope shape, not the piton shape.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional_court seat the arrangement is a settled bright line it administers — rule-like, low-variance, legitimacy-conferring. From the immunocompromised seat the same structure operates as a refusal to protect: a guarantee everyone else holds, priced in their exposure. From the public_health_authority seat it is simultaneously a binding constraint (the mandate tool is gone) and a shelter (trust preserved, coercion-delegation and mandate liability offloaded). From the vaccine_decliner seat it is absolute protection at no cost. The engine computes these per-seat classifications from the structural data; the divergence between the court's rule-experience and the vulnerable's risk-experience is the perspectival gap this kernel exists to adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: bodily_autonomy_claimants and vaccine_decliners hold mobile exit and full protection (d near the beneficiary end); civil_liberties_organizations collect mission, standing, and funding from the rule's operation and are identity-locked to it (d near the beneficiary end). The victim declaration drives high d: immunocompromised_individuals are trapped — their condition pins them to the exposure, no arbitrage exists (d near the full-target end). One override is authored for the institutional power atom (d = 0.35): the flat derivation would seat public_health_authorities deep in the beneficiary range (~0.15) on their beneficiary declaration alone, but their position is genuinely dual — they lose the strongest outbreak-control tool while gaining trust-preservation and liability shelter — and constitutional_courts expend enforcement effort while collecting doctrinal authority. The override seats the institutional atom between beneficiary and symmetric, which the role lists alone cannot express.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is bidirectional. The categorical rhetoric ('impermissible regardless of collective benefit') invites a mountain reading — an absolute, natural-law-flavored limit — but the structural data defeat it: the rule does not persist unenforced, identifiable parties collect from its operation, identifiable parties pay for it, and it must be actively defended against recurrent legislative override. That is a constructed, enforced norm with a coordination function and asymmetric costs — tangled_rope, not mountain. The reverse mislabel is equally available: the victim set invites a snare reading (someone pays), but the coordination function is genuine and primary — a bright-line bodily-integrity rule solves a real collective problem no balancing regime solves as cheaply, and the rule predates its current cost structure — so this is not extraction wearing coordination as cover. On the genealogy: the founding problem (balancing frameworks licensing atrocity) is not dead — the rule has not outlived its function — so no mandatrophy resolution is declared; but the status is contested, because the mandate coalition attests that the founding problem has inverted into under-protection of the vulnerable. Read against the world_rearranges verdict, the arrangement's persistence is actively defended, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the bodily_autonomy_primary reading of the coercion_legitimacy_boundary kernel. Is the categorical side-constraint framing the only defensible instantiation of the kernel, or do the sibling readings (public_health_primary, proportionality_reading) instantiate equally defensible constraints with different victim sets and different epsilon over the same referent?',
    'Framing-level adjudication: which premise a polity''s constitutional settlement actually embeds, and whether the categorical form is load-bearing or rhetorical. Each sibling is authored as its own story with its own epsilon over the same standing arrangement; this story''s epsilon holds only within the categorical framing.',
    'Adopting public_health_primary flips the structure: vaccine_decliners become the constrained party, immunocompromised_individuals move to the protected beneficiary set, and epsilon rises with enforced compulsion machinery. Adopting proportionality_reading makes victim and beneficiary sets severity-indexed. The disagreement is located precisely at whether the bodily-integrity boundary is a side-constraint or a balanceable value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer position: one reading of the coercion-legitimacy kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    categorical_form_vs_carveouts,
    'Does the categorical rule hold categorically in operation, or do systematic carve-outs (involuntary psychiatric treatment, child-welfare medical overrides, emergency detention-treatment, prison and military medical regimes) already make it a proportionality rule in disguise?',
    'Systematic audit of nonconsensual-intervention statutes, case outcomes, and emergency powers across the jurisdiction: enumerate the classes of bodies the categorical rule does not reach, and determine whether the interpretive layer reclassifies them as scope questions rather than breaches of the kernel.',
    'If carve-outs are systematic, the constraint already operates as the proportionality reading: theater_ratio rises, the categorical form functions as cover, and this reading''s foreclosure edges overstate its displacement of the siblings. If carve-outs are marginal, the bright line is load-bearing and the coordination claim stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_form_vs_carveouts, empirical, 'Whether the categorical form survives contact with operational carve-outs.').

omega_variable(
    victim_set_withhold_rescue_boundary,
    'Are immunocompromised individuals victims OF this constraint, which imposes exposure by removing the mandate pathway, or victims of disease whom the constraint merely fails to rescue — is withholding a protection extraction at all?',
    'Conceptual adjudication of the extraction referent: whether a constraint that declines to transfer a protection counts as extracting from those who needed it, tested against the framework''s victim definition and against the sibling readings'' contrary accounting of the same population.',
    'Under the withhold-rescue framing the victim set dissolves, epsilon falls toward pure-coordination levels, and the classification moves toward rope. Under the imposition framing the victim set stands and the tangled_rope structure (genuine coordination plus asymmetric cost plus active enforcement) is confirmed. The authored structural delta assumes the imposition framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_withhold_rescue_boundary, conceptual, 'Boundary of the victim set: imposed exposure versus withheld rescue.').

omega_variable(
    voluntary_uptake_trust_premium,
    'Does the categorical rule actually deliver the trust-preservation and uptake benefits that qualify public_health_authorities as beneficiaries — or would mandate-capable regimes achieve equal or better uptake without trust collapse, flipping the authorities to pure payers?',
    'Comparative analysis of voluntary versus mandate regimes across jurisdictions and outbreak episodes: uptake trajectories, trust surveys, and exemption-driven backlash following mandate adoption.',
    'If the trust premium is real, the authority seat holds its qualified beneficiary position and the constraint''s coordination function is broader than the bright line alone. If not, the beneficiary set shrinks to claimants, decliners, and the liberties bar, the authority seat''s directionality rises toward full target, and the extraction asymmetry sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_uptake_trust_premium, empirical, 'Empirical basis of the mandate-enforcer beneficiary position declared in this reading''s structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(coer_tr_t0, observed).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(coer_tr_t10, observed).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(coer_tr_t20, observed).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(coer_tr_t30, observed).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(coer_tr_t40, observed).
narrative_ontology:measurement(coer_tr_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(coer_tr_t50, observed).
narrative_ontology:measurement(coer_tr_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(coer_tr_t60, observed).
narrative_ontology:measurement(coer_tr_t70, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 70, 0.16).
narrative_ontology:measurement_basis(coer_tr_t70, observed).
narrative_ontology:measurement(coer_tr_t78, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 78, 0.15).
narrative_ontology:measurement_basis(coer_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(coer_be_t0, observed).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(coer_be_t10, observed).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(coer_be_t20, observed).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.34).
narrative_ontology:measurement_basis(coer_be_t30, observed).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(coer_be_t40, observed).
narrative_ontology:measurement(coer_be_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 50, 0.4).
narrative_ontology:measurement_basis(coer_be_t50, observed).
narrative_ontology:measurement(coer_be_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(coer_be_t60, observed).
narrative_ontology:measurement(coer_be_t70, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 70, 0.45).
narrative_ontology:measurement_basis(coer_be_t70, observed).
narrative_ontology:measurement(coer_be_t78, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 78, 0.48).
narrative_ontology:measurement_basis(coer_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(coer_su_t0, observed).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(coer_su_t10, observed).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(coer_su_t20, observed).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.27).
narrative_ontology:measurement_basis(coer_su_t30, observed).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(coer_su_t40, observed).
narrative_ontology:measurement(coer_su_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 50, 0.37).
narrative_ontology:measurement_basis(coer_su_t50, observed).
narrative_ontology:measurement(coer_su_t60, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 60, 0.4).
narrative_ontology:measurement_basis(coer_su_t60, observed).
narrative_ontology:measurement(coer_su_t70, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 70, 0.43).
narrative_ontology:measurement_basis(coer_su_t70, observed).
narrative_ontology:measurement(coer_su_t78, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 78, 0.45).
narrative_ontology:measurement_basis(coer_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The kernel coercion_legitimacy_boundary decomposes into three readings with distinct epsilon, victim sets, and enforcement profiles. This reading (bodily_autonomy_primary) holds the boundary as a categorical side-constraint: immunocompromised individuals enter the victim set as exposed to the unvaccinated, mandate enforcers sit in a qualified beneficiary position, and epsilon is moderate because no compulsion machinery runs. public_health_primary flips the structure — decliners become the constrained party, the immunocompromised become protected beneficiaries, and epsilon rises with enforced compulsion. proportionality_reading interpolates, indexing victim and beneficiary sets to disease severity. The readings are siblings held by different coalitions, but this reading's categorical premise logically excludes both siblings' permissibility premises, which is recorded as foreclosure edges in cs_structure.reading_relations. The citation pattern runs against the foreclosure direction: the mandate lineage cites settled Jacobson-era doctrine as authority, while this reading cites the Nuremberg-lineage ethics documents as trumping authority — the family link preserves that contested provenance for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
