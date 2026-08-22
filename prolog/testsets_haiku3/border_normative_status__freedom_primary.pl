% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Enforcement as Freedom Constraint (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   Under the freedom-primary reading of border-normative status, freedom of
 *   movement is understood as a fundamental human right that borders
 *   impermissibly restrict. State exclusion requires extraordinary
 *   justification that the reading asserts is rarely (if ever) satisfied.
 *   Border enforcement is reframed from a neutral administrative mechanism
 *   into a structure for systematic rights violation. Excluded migrants are
 *   not merely inconvenienced; they are victimized by the constraint itself.
 *   Displaced domestic workers become victims because border barriers to
 *   economic migration violate the movement right. The founding
 *   problem—territorial coordination under conditions of rare movement—is
 *   declared solved and persistent, making border restriction a pure
 *   extraction mechanism riding on an obsolete coordination narrative. The
 *   theater ratio rises over the interval as security rhetoric intensifies
 *   while underlying exclusionary function persists: border agencies
 *   increasingly justify control through counterterrorism and national
 *   security discourse rather than through candid acknowledgment of
 *   labor-market gatekeeping and demographic control. Extractiveness rises as
 *   enforcement machinery tightens (visa requirements intensify, deportation
 *   machinery expands, mobility costs rise through fees and administrative
 *   friction).
 *
 * KEY AGENTS:
 *   - Excluded migrants: the primary victims under this reading; their exclusion is the constraint's core operation.
 *   - Asylum seekers and refugees: secondary victims whose claims for protection are adjudicated by the same apparatus that denies movement rights.
 *   - Displaced domestic workers: victims of labor-mobility restriction; included in victim set under freedom-primary reading (excluded in sovereignty-primary reading).
 *   - Nation-state apparatus: agenda-setter; administers and justifies exclusion through sovereignty rhetoric.
 *   - Destination-state citizens: seated ambiguously as beneficiaries of scarcity rent but harmed by reduced mobility options.
 *   - Origin-state governments: constrained negotiators; lose authority and remittance revenue.
 *   - Human rights advocacy organizations: observers and epistemic authorities for the freedom-primary reading.
 *   - Sovereignty-primary states: explicitly excluded from this reading's frame by the logical contradiction between readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.82).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.88).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Enforcement as Freedom Constraint (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '7d015d00-3c24-484d-84f8-b8b47b4f68e0').
narrative_ontology:cs_kernel_codification('7d015d00-3c24-484d-84f8-b8b47b4f68e0', distributed).
narrative_ontology:cs_authority_grounding('7d015d00-3c24-484d-84f8-b8b47b4f68e0', distributed).
narrative_ontology:cs_reading_relation('7d015d00-3c24-484d-84f8-b8b47b4f68e0', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('7d015d00-3c24-484d-84f8-b8b47b4f68e0', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('7d015d00-3c24-484d-84f8-b8b47b4f68e0', foundational, movement_is_foundational_right).
narrative_ontology:cs_axiom_status(movement_is_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('7d015d00-3c24-484d-84f8-b8b47b4f68e0', movement_is_foundational_right, deontological).
narrative_ontology:cs_axiom('7d015d00-3c24-484d-84f8-b8b47b4f68e0', foundational, borders_restrict_foundational_right).
narrative_ontology:cs_axiom_status(borders_restrict_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('7d015d00-3c24-484d-84f8-b8b47b4f68e0', borders_restrict_foundational_right, deontological).
narrative_ontology:cs_reference_frame('7d015d00-3c24-484d-84f8-b8b47b4f68e0', universal_movement_freedom).
narrative_ontology:cs_drift_state('7d015d00-3c24-484d-84f8-b8b47b4f68e0', contemporary_state_system, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('7d015d00-3c24-484d-84f8-b8b47b4f68e0', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, nation_state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_state_citizens).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, destination_state_citizens).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, origin_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons born outside a territorial boundary who seek entry or transit but are barred by border enforcement. Cannot exit the constraint (trapped at the perimeter); exit options are denied by the same borders that exclude them. Under the freedom-primary reading, their exclusion is a fundamental rights violation requiring extraordinary—rarely satisfied—justification. No legitimate escape path exists; the constraint's logic declares their exclusion arbitrary unless proven otherwise.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Persons fleeing persecution or violence who claim protection at borders and face lengthy adjudication, deportation, or warehousing in liminal spaces. Identity as a person fleeing harm cannot be exited—the claim persists as long as origin conditions persist. Border enforcement treats asylum claims as discretionary rather than rights-activating. Repeated denial activates the rights-violation doctrine under the freedom-primary reading.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, identity_locked, global).

% Persons formally recognized as fleeing persecution but confined to camps, restricted labor access, or perpetual temporary status. Refugee identity persists as long as home conditions remain unsafe (identity_locked). Even after formal recognition, border enforcement maintains restriction on movement and settlement. Under the freedom-primary reading, refugee containment violates the foundational right to movement; formal status confers no exit from the constraint because the border regime persists.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, refugees, payer,
    powerless, biographical, identity_locked, global).

% Domestic citizens displaced by economic shifts who would migrate for work but are blocked by visa restrictions, labor quotas, or mobility fees. Citizenship grants nominal movement rights nullified by border enforcement. They are victims under the freedom-primary reading because border barriers to economic migration are recognized as rights restrictions, not legitimate policy tools.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, biographical, constrained, national).

% The institutional complex (border agencies, visa systems, immigration courts, deportation enforcement) that administers and perpetuates border controls. Sets rules about entry, settles legitimacy claims by invoking sovereignty, deploys enforcement machinery to maintain exclusion. Collects political legitimacy, economic rents (visa fees, remittance taxation, labor-market regulation), and institutional power. The freedom-primary reading declares this entire edifice a structure for rights violation, not legitimate governance.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, nation_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, universal).

% Citizens of destination states with restrictive immigration policy who benefit from labor-market scarcity (higher service-sector wages, greater job selectivity) and symbolic border control. However, the freedom-primary reading treats this benefit as illusory: restrictions raise wages for some but eliminate labor mobility options for all, reduce opportunity for low-skill workers to migrate when domestic labor markets fail, and impose cultural/political fragmentation costs. Seated ambiguously between beneficiary and payer.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_state_citizens, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, destination_state_citizens, payer).

% Governments of countries from which migration flows occur. Lose remittance revenue and educated-emigrant labor if border restrictions tighten; face domestic pressure from citizens unable to move. They negotiate visa treaties from weak bargaining position. Border enforcement in destination states constrains their own authority to regulate departure.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, origin_state_governments, payer,
    moderate, generational, constrained, national).

% NGOs, UN bodies, and international legal experts who monitor border enforcement and advocate for rights-based migration frameworks. They document violations, file complaints, and pressure for treaty reform. Their analytical seat allows them to name the constraint and its operation; they neither benefit nor pay directly but serve as epistemological authority for the freedom-primary reading's empirical claims.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Governments holding the sovereignty-primary reading—that states have foundational authority to exclude on grounds of collective self-determination alone. They are excluded from the freedom-primary frame by definition: their core premise (borders are legitimate tools of state power) logically conflicts with the freedom-primary premise (movement is a foundational right). They argue the freedom-primary reading misunderstands political community and that exclusion is justified by membership, not by extraordinary demonstration of harm.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sovereignty_primary_states, excluded,
    institutional, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, nation_state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Borders ostensibly coordinate security screening (identifying threats), labor-market regulation (controlling wage dynamics via supply restriction), public-service allocation (managing fiscal burden), and political/cultural identity maintenance. Under the freedom-primary reading, these are reframed as coordination failures: security can be achieved through explicit screening without movement restriction; labor regulation can be transparent; services can be universalized or explicitly rationed; identity persists through culture and law, not confinement.
% TRANSFER_FUNCTION: Borders transfer autonomous choice about movement FROM individuals TO state institutions. Individuals lose the right to choose where to live, work, and move; states gain control over labor supply, demographic composition, and political narrative. The constraint also transfers the invisible goods: wage suppression in low-wage labor markets (from which origin-state workers benefit), fiscal burden shifting (from host-state residents to migrants), legitimacy narrative (from explicit power to neutral administration).
% ABSENT_VOICES: Excluded migrants are kept outside the polity by definition. Origin-state governments negotiate from structural weakness. International labor organizations are fragmented by national jurisdiction. Future generations who would adapt to climate change through mobility are not seated. The freedom-primary reading highlights this: the most fundamental policy affecting billions is decided without their voice, violating the very autonomy principle the reading grounds itself in.
% DISAPPEARANCE_RATIONALE: Labor mobility would increase substantially, wage structures would equilibrate, public services would require renegotiation, political communities would reorganize around voluntary association, security screening would be explicit rather than exclusion-by-geography, and the entire institutional apparatus of border enforcement would dissolve or transform into facilitation. The freedom-primary reading asserts this rearrangement is obligatory because it honors autonomy.
% FOUNDING_PROBLEM: Movement was historically restricted by technology and geography. State borders emerged to organize collective governance in defined territories when cross-border presence was rare and costly. The founding problem was coordination: how to organize collective decisions and services within bounded communities under conditions of low mobility and high communication cost.
% FOUNDING_PROBLEM_CORROBORATION: Modern transportation and communications have reduced movement costs to near-zero for resource-rich agents. States themselves acknowledge this through visa permits and labor-migration programs: they prove movement can be managed without blanket exclusion. UN agencies, academic migration research, and international economic data from outside benefiting governments attest the founding problem is solved. The EU Schengen zone directly demonstrates that coordination proceeds with minimal border friction. The freedom-primary reading: where borders are removed and coordination survives, the exclusion mechanism is shown unnecessary, proving persistence is political choice, not functional requirement.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers decision-making authority from individuals to states, restricts the most basic dimension of autonomy (where to live), and persists despite solved founding problem. Suppression is higher still (0.88) because border enforcement requires continuous coercive machinery: walls, patrols, detention, deportation. Exit from the constraint is blocked for powerless agents (trapped, identity_locked). Accessibility_collapse is moderate-high (0.71) because the border constraint is, by definition, a reduction of alternatives to movement—yet alternatives to movement exist (internal migration, remote work, political advocacy for border opening), so collapse is not total. Theater_ratio rises over the interval (0.28 to 0.42) because border enforcement increasingly justifies itself through security and cultural narratives while the underlying extraction (labor-market gatekeeping, fiscal burden shifting to migrants) proceeds invisibly. The constraint meets substantial resistance (0.73) from migrants who attempt unauthorized crossing, civil-society organizations that advocate for migration rights, and academic-expert communities that argue borders are unjust. Resistance is not higher because the institutional power asymmetry is extreme and migrants are dispersed and individually weak. The claim (snare) reflects the reading's assessment: this is pure extraction (exclusion and rights restriction) wearing the costume of coordination (security, service provision, identity maintenance).
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty-primary reading would seat this constraint as legitimate governance (rope or tangled_rope: real security/coordination functions, active enforcement needed but justified). The qualified-sovereignty reading would acknowledge the extraction but argue for *proportionality* (extraction is permissible if constrained by necessity and rights protections). The freedom-primary reading rejects both: it asserts that movement is a fundamental right and borders that restrict it are inherently extractive regardless of their claimed coordination functions. The engine should compute: sovereign and qualified readings score as institutional beneficiary (state apparatus); freedom-primary reading scores as extractor abusing power. This divergence is structural, not empirical—it reflects different commitments about what counts as a legitimate restriction on autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the freedom-primary reading, directionality is determined by the movement right and whether the agent can exercise it. Excluded and trapped agents have d approaching 1.0. Institutional agenda-setters have d approaching 0.0 (beneficiaries). This is a highly asymmetric structure: millions of low-power payers, one institutional beneficiary. No directionality override needed; structural data produces appropriate values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination under low-mobility conditions) is structurally dead: modern transportation eliminates the original justification. Yet the constraint persists and intensifies (extractiveness rising, theater rising, enforcement tightening). This is a textbook mandatrophy case: the justification no longer obtains, but the institutional machinery persists because it serves extraction interests. The freedom-primary reading declares the constraint a zombie—a structure that no longer serves its founding function and persists purely through institutional inertia and power maintenance. The engine should flag this constraint as a candidate for mandatrophy resolution under the freedom-primary reading. (Note: sovereignty-primary and qualified-sovereignty readings would reject this analysis, asserting that borders serve ongoing coordination functions beyond the founding problem. The mandatrophy verdict is reading-dependent.)
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_right_vs_community_membership,
    'Is freedom of movement a pre-political individual right, or is the right to exclude (via borders) constitutive of what political community means?',
    'Philosophical analysis of competing foundations for rights claims; empirical data on whether communities organized without exclusion (EU Schengen, open borders within federal systems) maintain political legitimacy and cohesion.',
    'If movement is foundational, the freedom-primary reading''s classification holds. If community-membership requires boundary-drawing authority, the sovereignty-primary reading''s classification holds. If both can coexist under proportionality conditions, the qualified-sovereignty reading holds. This omega frames the entire kernel dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_right_vs_community_membership, conceptual, 'Whether freedom of movement is foundational or derivative from political community.').

omega_variable(
    empirical_status_founding_problem,
    'Is the founding problem (coordination under low-mobility conditions) empirically dead, or do modern borders still solve coordination problems that would otherwise emerge?',
    'Comparative analysis of border openness and institutional performance: do jurisdictions with minimal borders (Schengen, internal US movement, pre-COVID airline access) experience coordination failure? Do closed-border jurisdictions experience coordination success measurably better than open-border regions?',
    'If founding problem is dead, mandatrophy resolution applies and persistent borders are pure extraction. If borders still solve coordination problems, they retain functional justification under the freedom-primary reading''s own terms (though the reading would still argue the constraint is unjust, the institutional defense becomes harder).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_status_founding_problem, empirical, 'Whether border closure still solves the coordination problem it was founded to solve.').

omega_variable(
    labor_market_extraction_mechanism,
    'How much of the extracted value from border-enforced labor-market restriction is consciously intended vs. an incidental effect of territorial-control enforcement?',
    'Analysis of legislative intent, administrative guidance, and rhetoric from border agencies and destination-state governments; disclosure of labor-market impact analysis in border policy.',
    'If intentional, the constraint is more clearly predatory (snare confirmed). If incidental, the classification remains snare but the reputational and political dynamics shift—the constraint may be more negotiable if framed as unintended side effect rather than deliberate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_extraction_mechanism, empirical, 'Whether border enforcement''s labor-market effects are intentional extraction or incidental to territorial control.').

omega_variable(
    identity_fusion_in_citizenship,
    'For destination-state citizens, is the connection between border closure and personal identity so fused that border opening would trigger existential threat responses (identity_locked), or is this connection contingent and teachable?',
    'Longitudinal survey research on how border attitudes shift with exposure to open-border narratives; analysis of attitude change before/after open-border events (Schengen accession, Brexit vote reversals, US-Canada open-border proposals).',
    'If identity-fused, destination-state citizens are actually identity-locked payers, not beneficiaries—their opposition to migration is driven by defensive identity maintenance, not rational preference for scarcity rent. This reclassifies them from ambiguous beneficiaries to trapped victims. If contingent, they are mobile agents who could shift positions with evidence and narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_citizenship, empirical, 'Whether destination-state citizens are identity-locked to border closure or capable of position change.').

omega_variable(
    kernel_reading_contestation,
    'Is the freedom-primary reading a coherent foundational premise about rights, or is it an aspirational claim that necessarily conflicts with other legitimate values (community, security, sovereignty)?',
    'Logical and philosophical analysis of whether the freedom-primary axiom (movement is a fundamental right that borders violate) can coexist with legitimate state interests without requiring absolute borderlessness, which most political communities reject.',
    'If coherent and universal, the reading''s classification holds absolutely (snare, mandatrophy). If it conflicts with other legitimate values, the kernel is genuinely contested rather than resolvable, and all three readings hold partial truths—classification becomes reading-dependent rather than objective. The freedom-primary reading is built on the assumption of coherence; this omega documents the unresolved philosophical question beneath that assumption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether freedom of movement can be made foundational without dissolving legitimate state interests in community and security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__freedom_primary, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(bord_tr_t8, observed).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__freedom_primary, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(bord_tr_t16, observed).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__freedom_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(bord_tr_t24, observed).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__freedom_primary, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(bord_tr_t32, observed).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(bord_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t8, border_normative_status__freedom_primary, base_extractiveness, 8, 0.71).
narrative_ontology:measurement_basis(bord_be_t8, observed).
narrative_ontology:measurement(bord_be_t16, border_normative_status__freedom_primary, base_extractiveness, 16, 0.75).
narrative_ontology:measurement_basis(bord_be_t16, observed).
narrative_ontology:measurement(bord_be_t24, border_normative_status__freedom_primary, base_extractiveness, 24, 0.79).
narrative_ontology:measurement_basis(bord_be_t24, observed).
narrative_ontology:measurement(bord_be_t32, border_normative_status__freedom_primary, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(bord_be_t32, observed).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(bord_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t8, border_normative_status__freedom_primary, suppression_requirement, 8, 0.81).
narrative_ontology:measurement_basis(bord_su_t8, observed).
narrative_ontology:measurement(bord_su_t16, border_normative_status__freedom_primary, suppression_requirement, 16, 0.84).
narrative_ontology:measurement_basis(bord_su_t16, observed).
narrative_ontology:measurement(bord_su_t24, border_normative_status__freedom_primary, suppression_requirement, 24, 0.86).
narrative_ontology:measurement_basis(bord_su_t24, observed).
narrative_ontology:measurement(bord_su_t32, border_normative_status__freedom_primary, suppression_requirement, 32, 0.87).
narrative_ontology:measurement_basis(bord_su_t32, observed).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(bord_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.18).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% The constraint 'border_normative_status' decomposes into three structurally distinct constraint stories, one for each reading of the contested kernel: freedom_primary (this file) treats borders as rights violations; qualified_sovereignty treats borders as permissible if constrained by proportionality; sovereignty_primary treats borders as legitimate tools of collective self-determination. Each reading instantiates a different constraint (different ε, different beneficiary/victim structure, different type). They are linked via network.affects_constraints because each reading's legitimacy claims depend partly on refuting or accommodating the others. The freedom-primary reading's core axiom (movement is foundational) directly challenges the sovereignty-primary axiom (exclusion is legitimate); they coexist as live positions held by different political movements but neither can be integrated into a single framework—they are sibling readings, not alternative measurements of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
