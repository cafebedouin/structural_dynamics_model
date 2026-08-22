% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope Under Enforcement Vacuum
 *   domain: legal/intellectual_property/open_source
 *
 * SUMMARY:
 *   The GPL is a kernel commitment: a stabilized text (the GNU General Public
 *   License) that grounds open-source governance. But Section 2(b)'s
 *   definition of 'derivative work' was written in 1989 for source-code
 *   modification in a simpler era and is ambiguous when applied to dynamic
 *   linking, plugins, and distributed systems. No binding court precedent has
 *   resolved this ambiguity. This reading instantiates the constraint as it
 *   operates in the absence of that precedent: a tangled
 *   coordination-and-extraction arrangement where interpretive communities
 *   with local enforcement capacity impose their reading (strong-copyleft,
 *   narrow-scope, or hybrid) in their domains, and adopters navigate by risk
 *   assessment and community alignment. Clarity-seeking adopters bear
 *   elevated costs; pragmatic adopters exploit the ambiguity. The constraint
 *   persists because the kernel uncertainty itself is structural—neither
 *   community can unilaterally bind the other, and courts have not stepped in
 *   to resolve the question.
 *
 * KEY AGENTS:
 *   - FSF-aligned projects: interpret GPL as strong copyleft; enforce in kernel and GNU ecosystems
 *   - Industry-dominated ecosystems: interpret GPL narrowly; enforce in corporate and proprietary-hybrid contexts
 *   - Pragmatic adopters: exploit ambiguity to choose interpretation that fits their business model
 *   - Clarity-seeking adopters: incur high costs to determine the 'right' interpretation before building
 *   - Subordinate communities: forced to adopt dominant interpretation in their upstream
 *   - Courts: absent—their silence is the structural feature; a binding ruling would collapse the plurality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.42).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope Under Enforcement Vacuum").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "legal/intellectual_property/open_source").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'fdf2a001-fe8e-48ed-ab28-3397bf730d68').
narrative_ontology:cs_kernel_codification('fdf2a001-fe8e-48ed-ab28-3397bf730d68', fixed_text).
narrative_ontology:cs_authority_grounding('fdf2a001-fe8e-48ed-ab28-3397bf730d68', distributed).
narrative_ontology:cs_reading_relation('fdf2a001-fe8e-48ed-ab28-3397bf730d68', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdf2a001-fe8e-48ed-ab28-3397bf730d68', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('fdf2a001-fe8e-48ed-ab28-3397bf730d68', foundational, absence_of_precedent_enables_plurality).
narrative_ontology:cs_axiom_status(absence_of_precedent_enables_plurality, holdable).
narrative_ontology:cs_axiom_grounding('fdf2a001-fe8e-48ed-ab28-3397bf730d68', absence_of_precedent_enables_plurality, empirically_contingent).
narrative_ontology:cs_axiom('fdf2a001-fe8e-48ed-ab28-3397bf730d68', foundational, interpretive_community_enforcement_capacity_local).
narrative_ontology:cs_axiom_status(interpretive_community_enforcement_capacity_local, holdable).
narrative_ontology:cs_axiom_grounding('fdf2a001-fe8e-48ed-ab28-3397bf730d68', interpretive_community_enforcement_capacity_local, conventional).
narrative_ontology:cs_reference_frame('fdf2a001-fe8e-48ed-ab28-3397bf730d68', gpl_pluralist_interpretation).
narrative_ontology:cs_drift_state('fdf2a001-fe8e-48ed-ab28-3397bf730d68', contemporary_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fdf2a001-fe8e-48ed-ab28-3397bf730d68', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, interpretive_community_with_local_enforcement_capacity).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, subordinate_interpretive_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, free_software_movement).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, interpretive_pluralism_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__enforcement_vacuum_reading, community_enforcement_variance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret GPL Section 2(b) expansively (strong copyleft reading): any linkage or coupling triggers GPL obligation. Enforce this reading through community peer pressure, licensing reviews, and threat of fork or exclusion. Control interpretive authority in FSF-aligned governance contexts (kernel projects, GNU ecosystem). Their interpretation shapes what counts as a derivative work for projects in their domain.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects, agenda_setter,
    organized, generational, constrained, global).

% Interpret GPL Section 2(b) narrowly (narrow scope reading): only direct source-level modifications count as derivatives; dynamic linking, plugins, and aggregation are separate works. Enforce through legal counsel, licensing audits, and corporate policy. Control interpretive authority in corporate/proprietary-hybrid ecosystems (Android vendors, embedded systems, cloud stacks). Their interpretation licenses different behavior in different contexts.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, industry_dominated_ecosystems, beneficiary).

% Navigate the interpretive plurality by selecting the interpretation that fits their business model and community alignment. Exploit the absence of binding precedent to adopt whichever reading enables their target architecture. Face lower legal risk because no court has definitively resolved the question; can credibly claim reliance on a reasonable reading. Examples: embedded Linux vendors adopting narrow-scope, cloud-native projects adopting strong-copyleft depending on their customer base.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Want definitive guidance on what GPL requires before building architectures. Incur elevated transaction costs: legal review, community vetting, pre-commitment to one interpretation, risk of incompatibility if they choose wrong. Cannot exploit the ambiguity without unacceptable uncertainty. Face pressure from both interpretive communities to adopt their reading, and must defend their choice against challenge from the other.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters, payer,
    moderate, biographical, constrained, national).

% Small open-source projects or niche ecosystems (embedded systems, academic contexts, regional software communities) that lack independent enforcement capacity. Forced to adopt whichever interpretation the upstream or dominant community (FSF, industry standard-setter, or major distributor) enforces locally. Face exclusion risk if they adopt the 'wrong' reading for their context. Lack resources for independent legal defense of their interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, subordinate_interpretive_communities, payer,
    powerless, biographical, identity_locked, local).

% Would argue for the narrowest possible GPL scope or for exemption from copyleft if admitted. They are excluded from the interpretive communities that maintain the constraint; their licensing strategy depends on avoiding GPL code entirely or using narrow-scope workarounds. The absence of precedent allows them to push boundaries via technical architecture.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, software_vendors_with_proprietary_components, excluded,
    powerful, biographical, arbitrage, global).

% Have not issued definitive precedent that resolves the copyleft scope question. Their absence is the structural fact this reading turns on: no binding judicial precedent means interpretive communities have local enforcement authority instead. If a court were to rule definitively, it would collapse the plurality and eliminate this constraint (enforcing one reading into dominance).
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, courts_and_legal_authorities, observer,
    institutional, generational, analytical, national).

% Benefits from the constraint's existence: the interpretive plurality keeps the question of copyleft scope alive in practice, preventing proprietary-friendly narrow readings from becoming the default. The absence of precedent prevents a court from narrowing GPL's scope, which would weaken the free software guarantee. Maintains generative pressure through interpretive advocacy and community enforcement.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, free_software_movement, beneficiary,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_adopters).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared commitment (GPL licensing) whose operative scope is negotiated through community interpretation and enforcement, allowing multiple legitimate readings to coexist without judicial settlement. Solves the problem of licensing ambiguity by distributing interpretation authority to communities with enforcement capacity in their domains, avoiding single-source gatekeeping while maintaining the GPL's force.
% TRANSFER_FUNCTION: Transfers cognitive and legal labor (determining what GPL means in practice) from courts and licensing authorities to interpretive communities and adopters. Pragmatic adopters shift risk assessment work to clarity-seeking adopters; dominant communities impose their reading locally; smaller communities absorb compliance costs to stay compatible.
% ABSENT_VOICES: Proprietary software vendors and users who benefit from narrow GPL scope are structurally excluded—they cannot participate in FSF-aligned or industry interpretive communities without accepting the GPL obligation. Courts, whose definitive interpretation would collapse the plurality, are also effectively absent (their silence is the structural feature).
% DISAPPEARANCE_RATIONALE: If the enforcement vacuum closed—either through definitive court precedent or through consensus agreement—the interpretive plurality would collapse. Projects would reorganize around a single stable reading (likely narrow-scope from institutional pressure or strong-copyleft from FSF enforcement). Adopters would no longer navigate by community alignment; they would follow a binding rule. The entire risk-management landscape for GPL adoption would shift.
% FOUNDING_PROBLEM: GPL Section 2(b) uses the term 'derivative work' without defining it for software contexts (source-level modification only? any coupling? dynamic linking?). Copyright law was written before software existed; the term's application to code linkage, plugins, and distributed systems is genuinely ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Supported by: (1) absence of binding judicial precedent in major jurisdictions (US courts have never definitively ruled on GPL's copyleft scope for dynamic linking or plugins); (2) ongoing disagreement between FSF legal counsel and commercial open-source firms (documented in public statements, litigation threats, and licensing policy divergence); (3) academic legal commentary noting the unresolved status (Moglen, Rosen, Stallman-era FSF guidance, and subsequent institutional shifts all attest the question is live and contested, not settled).
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38): the constraint transfers cognitive/legal labor from courts to communities, and pragmatic adopters benefit from ambiguity while clarity-seekers lose. But extraction is not severe because adopters retain choice (they can select their interpretation and community alignment). Suppression is also moderate (0.42): it operates through community peer pressure and threat of fork/exclusion, not through coercive legal machinery (which does not exist at the scale of individual projects). Theater is low-to-moderate (0.28): the interpretive communities perform enforcement (licensing reviews, community vetting), but the theatrical element is minimal because the underlying commitment (the GPL text) is real and the ambiguity is genuine. The measurement series show a slight rise in extractiveness and theater as normalization occurs—both communities consolidate their interpretive authority over the interval—then stabilize as equilibrium sets in. Suppression requirement rises and then plateaus as enforcement capacity stabilizes in each community.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF seat, the constraint is genuine coordination (GPL licensing) with minor extraction (clarity-seeking costs are the price of openness). From the industry seat, it is also coordination (GPL licensing) with managed extraction (narrow scope maximizes their flexibility). From a clarity-seeking adopter's seat, it is pure extraction: they bear all the legal labor, face the risk of choosing 'wrong,' and gain no benefit from the ambiguity (they prefer a definitive rule). From a subordinate community's seat, it is coercive: they have no real choice of interpretation; their upstream decides. The engine will compute these divergences from the structural directionality data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF-aligned and industry-dominated communities are both agenda-setters: they set the meaning of GPL in their domains. Pragmatic adopters are beneficiaries (they exploit the ambiguity); clarity-seeking adopters and subordinate communities are targets (they absorb elevated costs and lose autonomy). The courts are observers whose absence enables the constraint. Directionality for pragmatic adopters is low (near beneficiary): they have mobile exit (can choose their community) and reap the benefit of flexibility. Directionality for clarity-seekers is high (near target): they are trapped between conflicting interpretations and must incur legal costs to navigate. Subordinate communities are trapped (identity_locked): their upstream determines their reading, and exit means forking or rewriting—costs that exceed their resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two misreadings: (1) reading this as pure rope (genuine coordination with symmetric costs) would miss that clarity-seekers and subordinate communities absorb asymmetric legal and compliance costs; (2) reading it as snare (pure extraction with suppression masking the true function) would miss that the GPL is a real shared commitment and the ambiguity genuinely does enable coordination through interpretive pluralism. The tangled_rope framing captures that this IS coordination—but asymmetric extraction rides on the same structure. The founding_problem_status (live) and disappearance_verdict (world_rearranges) together signal mandatrophy risk: if a court were to rule, the founding problem would be solved and the constraint would vanish, replaced by a single binding rule. The absence of precedent is the only thing keeping this arrangement in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_collapse_path,
    'Which court ruling or precedent, if established, would collapse the interpretive plurality and eliminate this constraint?',
    'A binding federal court decision (US) or ECJ ruling (EU) defining ''derivative work'' for software copyleft purposes. Alternatively, a deliberate amendment to the GPL itself that clarifies Section 2(b).',
    'A strong-copyleft precedent would establish the FSF reading as binding law, ending the plurality. A narrow-scope precedent would establish the industry reading. Either way, the enforcement vacuum closes and adopters follow a rule instead of navigating interpretive communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precedent_collapse_path, empirical, 'Path to collapse of the enforcement vacuum and elimination of this constraint.').

omega_variable(
    subordinate_community_coalescence,
    'Do powerless subordinate communities bear the extraction as a stable cost, or do they coalescence into a third interpretive community capable of enforcing their own reading?',
    'Historical observation: small open-source ecosystems (embedded systems, academic contexts, niche languages) that adopt a consistent interpretation and enforce it through their own distribution channels (package managers, certification, fork threats) rather than relying on FSF or industry authority.',
    'If subordinate communities form coalitions with independent enforcement capacity, they cease to be targets and become organizational-level agenda-setters, shifting the distribution of power. The constraint would remain plural but more balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinate_community_coalescence, empirical, 'Whether subordinate communities can organize collective enforcement capacity.').

omega_variable(
    clarity_seeking_cost_internalization,
    'Do clarity-seeking adopters remain clarity-seekers, or do they gradually accept the ambiguity and become pragmatic exploiters, internalizing the risk-assessment cost as a normal business expense?',
    'Observing whether clarity-seeking adopters shift their rhetoric and behavior over time: from demanding a definitive ruling to accepting the interpretive plurality as a feature (risk diversification, optionality) rather than a bug.',
    'If clarity-seekers internalize the cost, they shift from targets to pragmatic beneficiaries. The cost distribution would shift again, and the constraint would become more stable (fewer agents bearing extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarity_seeking_cost_internalization, empirical, 'Whether clarity-seeking costs become normalized or remain experienced as extraction.').

omega_variable(
    kernel_reading_committer_frame,
    'Is the enforcement_vacuum_reading itself a stability-seeking reframing that obscures underlying competitive dynamics, or does it accurately describe how GPL interpretation works when no court has ruled?',
    'Comparing the enforcement_vacuum_reading against the strong_copyleft_reading and narrow_scope_reading in terms of which reading better predicts adopter behavior, licensing conflict patterns, and interpretive community alignment over the interval.',
    'If the enforcement_vacuum_reading is a cover story for hidden competitive dynamics, then the true constraint may be closer to snare (pure competition masked as ambiguity). If it accurately describes the mechanism, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the enforcement vacuum framing is structurally accurate or a reframing that disguises underlying extraction.').

omega_variable(
    identity_lock_mechanism_subordinate_communities,
    'Is the identity-lock for subordinate communities structural (they cannot exit without rewriting all their code and losing compatibility) or internalized (they have come to believe the interpretive community''s reading is correct)?',
    'Post-exit observation: if a subordinate community forks and adopts a different GPL interpretation, do they maintain their former relationships and code compatibility, or do they face immediate technical and social friction that forces them back toward the dominant interpretation?',
    'If lock is structural, the extraction on subordinate communities is stable and sustained by technical coupling. If lock is internalized, a change in cultural framing (community leadership, values shift) could unlock the exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_subordinate_communities, empirical, 'Whether subordinate community lock is structural dependency or internalized commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t3, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t3, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t18, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t3, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement_basis(gpl__be_t3, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(gpl__be_t18, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(gpl__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t3, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement_basis(gpl__su_t3, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t18, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement_basis(gpl__su_t18, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(gpl__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel decomposes into three constraint stories, each representing a distinct reading of Section 2(b)'s 'derivative work' definition. The enforcement_vacuum_reading instantiates the constraint as it operates in the absence of binding judicial precedent—a tangled_rope where interpretive pluralism is itself the structural feature. The strong_copyleft_reading represents the FSF's expansive interpretation (would manifest as Snare or Tangled Rope from proprietary-software perspective). The narrow_scope_reading represents the industry's restrictive interpretation (would manifest as Rope or Mountain from proprietary perspective). All three stories share the same kernel text but diverge in their ε values, beneficiary/victim structures, and claimed types based on which reading's standpoint they adopt. The enforcement_vacuum_reading sits between the other two: it treats the absence of precedent as the operative constraint, not the disputed content of Section 2(b) itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, powerless, 0.78).
constraint_indexing:directionality_override(gpl_copyleft_scope__enforcement_vacuum_reading, moderate, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
