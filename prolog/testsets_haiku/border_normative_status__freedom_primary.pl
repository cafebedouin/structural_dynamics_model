% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Exclusion as Fundamental Rights Violation (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The freedom-primary reading asserts that freedom of movement is a
 *   fundamental human right that borders impermissibly restrict. Under this
 *   reading, border exclusion of migrants constitutes a rights violation that
 *   requires extraordinary justification — a justification that modern border
 *   regimes cannot provide because they are sustained by manufactured threat
 *   narratives, not genuine security. The constraint is a snare from this
 *   reading's perspective: it extracts labor control, political legitimacy,
 *   and nationalist coalition power from the powerless while theater
 *   (security, order, sovereignty) obscures the extraction. The reading
 *   treats excluded migrants as victims of a rights-violating constraint;
 *   displaced domestic workers are newly visible victims (their exclusion
 *   from migration opportunities is reads as rights violation); and the
 *   enforcement apparatus is the agenda-setter collecting rents through labor
 *   control and political authority. The measurement series shows rising
 *   extraction (0.68→0.82) and rising theater (0.28→0.42) over the interval,
 *   indicating the constraint's function is increasingly theatrical (the
 *   security justification is less credible) even as enforcement intensifies.
 *
 * KEY AGENTS:
 *   - Excluded migrants: powerless, trapped, victims of rights violation under the freedom-primary reading
 *   - Internally displaced workers: moderate power but constrained exit, newly visible victims whose movement rights are violated
 *   - Refugee populations: powerless, immediate horizon, dual victims (freedom + protection rights violated)
 *   - State border enforcement apparatus: institutional power, arbitrage exit, agenda-setter, administers and collects rents from exclusion
 *   - Incumbent citizens (nationalist framing): organized power, mobile exit, beneficiaries who receive misdirection of their grievance
 *   - Nationalist political coalitions: organized power, mobile exit, beneficiaries who collect political capital from exclusion narrative
 *   - Human rights advocacy sector: moderate power, mobile exit, structurally excluded from policy authority despite holding the reading
 *   - Cosmopolitan legal authority: institutional power, analytical exit, observer seat holding competing normative authority
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
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Exclusion as Fundamental Rights Violation (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'cd1be6ab-8d35-41a1-b129-06c0f3acd24d').
narrative_ontology:cs_kernel_codification('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', distributed).
narrative_ontology:cs_authority_grounding('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', extraction).
narrative_ontology:cs_interpretation_layer_present('cd1be6ab-8d35-41a1-b129-06c0f3acd24d').
narrative_ontology:cs_reading_relation('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', foundational, freedom_of_movement_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', freedom_of_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', universal_freedom_of_movement).
narrative_ontology:cs_drift_state('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', contemporary_enforcement_intensification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd1be6ab-8d35-41a1-b129-06c0f3acd24d', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, incumbent_citizens_nationalist_framing).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, state_border_enforcement_apparatus).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, internally_displaced_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, refugee_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, nationalist_political_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People denied border entry who claim fundamental right to freedom of movement. Under the freedom-primary reading, their exclusion is a rights violation. They are trapped in origin locations, unable to exit the constraint because borders deny them exit even when they flee violence or economic destitution. No formal appeals mechanism exists where they can contest exclusion as unjustified — the reading asserts they should have that standing.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Domestic workers displaced by economic collapse, climate disruption, or regional violence who attempt to relocate across borders seeking work. Face border enforcement framed as labor-market protection; under the freedom-primary reading, this enforcement violates their movement rights and forces them into informal economy or immobility. Their constraint is constrained exit (they can move internally but face barriers at international borders) rather than trapped.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, internally_displaced_workers, payer,
    moderate, biographical, constrained, national).

% People fleeing persecution or violence who seek asylum. Encounter borders that reject or delay asylum claims through legal procedure, resource starvation, and de facto exclusion. Under the freedom-primary reading, border denial of refuge seekers violates both freedom of movement AND protection rights. They are trapped in dangerous origin locations with no legitimate exit mechanism.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, refugee_populations, payer,
    powerless, immediate, trapped, global).

% Immigration agencies, border patrol, customs enforcement, immigration courts, and related state machinery. Administers exclusion rules, collects rents through deportation authority, visa fees, detention, and labor-control mechanisms. Justified by state-security framing. The freedom-primary reading reframes this institutional complex as extractive enforcement that violates rights and that persists because state apparatus benefits from it, not because it serves genuine coordination.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, state_border_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic populations, especially economically precarious ones, who benefit from border-closure framing because it misdirects responsibility for their economic condition. Told that their precarity is an invasion problem (migrant competition, outsourcing caused by immigration) rather than structural (automation, financialization, union decline). The constraint's theater tells them the story: migration caused their immiseration. This misdirection is the extraction the freedom-primary reading names — their genuine grievance is captured and redirected at the powerless rather than at the systems that actually caused it.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, incumbent_citizens_nationalist_framing, beneficiary,
    organized, generational, mobile, national).

% Political movements and parties that gain electoral power by framing migration as threat and border enforcement as protection. The constraint's theater (security, sovereignty, national interest) supplies their primary narrative. They collect political capital, coalition-building power, and electoral legitimacy from exclusion frames. The freedom-primary reading treats this as rents extracted from migrants and displaced workers — the constraint persists because these coalitions benefit from the extraction.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, nationalist_political_coalitions, beneficiary,
    organized, biographical, mobile, national).

% International human rights organizations, civil-society groups, and advocacy networks that endorse the freedom-primary reading and contest border enforcement as rights violation. Structurally excluded from state border policy-making despite producing competing narratives and shadow jurisdictional authority through UN bodies, treaty bodies, and courts. Their exclusion from policy authority is what the enforcement apparatus maintains — if their reading achieved institutional power, every border exclusion would have to justify itself against a freedom-of-movement presumption, and the current enforcement regime could not survive that standard.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_advocacy_sector, excluded,
    moderate, biographical, mobile, global).

% International courts (European Court of Human Rights, International Court of Justice), treaty bodies (UN Human Rights Committee), and legal scholarship that interpret border enforcement through human-rights and freedom-of-movement lenses. Hold competing normative authority that would require states to justify exclusion if the reading governed. They lack enforcement power but generate pressure and occasionally invalidate border practices through opinion.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, cosmopolitan_legal_authority, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, state_border_enforcement_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Border enforcement coordinates state security against external threat and controls population flows to domestic labor markets and welfare systems. It solves a collective-action problem among resident populations who wish to exclude non-members and maintain ingroup advantage. Under the freedom-primary reading, this coordination function is real but rests on a manufactured threat narrative (most excluded migrants are economic, not military threats) and hides the constraint's true function: capturing working-class grievance and misdirecting it at the powerless, while enforcement apparatus collects labor-control rents.
% TRANSFER_FUNCTION: Moves freedom of movement from excluded migrants, refugees, and internally displaced workers to incumbent citizens and nationalist political coalitions; transfers labor control (the ability to deny work authorization and reduce labor bargaining power) from workers to state enforcement apparatus; transfers political legitimacy and coalition-building power from human-rights framings to state-security framings; transfers economic blame for precarity from structural causes (automation, financialization) to migration and migrants.
% ABSENT_VOICES: Human rights advocacy organizations and cosmopolitan legal authority are structurally excluded from border policy authority. They would reframe the constraint as rights violation if seated with equal authority; their exclusion from policy-making is maintained by the same enforcement apparatus that sustains the constraint. Excluded migrants themselves have no voice in the framing — the constraint denies them standing even to contest their own exclusion as unjustified. Internally displaced workers are sometimes represented by labor organizations but are often invisible in border-policy discourse (the focus is on international migration, not internal displacement).
% DISAPPEARANCE_RATIONALE: If the freedom-primary reading were institutionalized and border enforcement required extraordinary, individually-justified justification for each exclusion, the current regime would collapse within years. Migration patterns would shift dramatically; labor markets would reorganize around market clearing rather than legal barriers; nationalist political coalitions would lose their primary narrative (the 'invasion' story would no longer organize politics); state enforcement apparatus would lose labor-control authority; incumbent citizens would face direct pressure to name and address the actual causes of their precarity (automation, financialization) rather than blaming migrants. The constraint persists because it serves all these beneficiary functions; if those extracted values disappeared, every institutional actor would have to reorganize its strategies.
% FOUNDING_PROBLEM: Interstate conflict and military threat in a Westphalian system of competing sovereigns, requiring states to control population flows during war and to maintain secure borders against invasion. The founding problem was legitimate security in an anarchic state system.
% FOUNDING_PROBLEM_CORROBORATION: State authorities claim the founding problem remains live, citing terrorism and security threats. However, systematic research on border-security efficacy shows modern borders face negligible military threat from migration; contemporary exclusion targets are overwhelmingly economic migrants and refugees, not invading armies or terrorists. The European border patrol, US border enforcement, and Australian offshore detention systems show patterns consistent with labor control and political scapegoating, not security response. Independent migration research (World Bank, OECD, academic demography) attests that migrants are not security threats and that exclusion is driven by labor-market politics and xenophobic narrative, not by genuine founding-problem persistence. The freedom-primary reading is corroborated by this evidence: the founding problem is dead, but the constraint persists.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.82) because the constraint transfers freedom of movement from excluded migrants to incumbent citizens and enforcement apparatus; the transfer is not reciprocal (migrants get nothing; enforcement apparatus gets labor control and authority; incumbent citizens get misdirected blame). Suppression is higher (0.88) because the constraint's persistence depends on actively excluding alternative framings (human rights reading) and denying migrants standing to contest their exclusion. Theater is moderate-to-high (0.42) because the security/order justification (theater) obscures the constraint's true function: labor control and political scapegoating. The measurement series show rising extractiveness and theater over the interval, indicating the constraint's security justification is eroding in credibility (theater rising) while enforcement is intensifying (suppression stable-high), a classic pattern of institutional capture where means are escalated because the legitimating narrative is failing. Accessibility collapse is moderate (0.71) because alternatives (cosmopolitan legal reading, human rights authority, open-border migration) are intellectually available but institutionally suppressed — migrants and displaced workers have no access to the competing reading at the point of enforcement. Resistance is high (0.73) because human rights advocacy, asylum seekers, and migrant-solidarity movements actively resist the constraint, though they lack enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   The freedom-primary reading produces the most extreme perspective divergence of any constraint in its kernel. From the state enforcement apparatus's seat, the constraint is genuine coordination (security function, necessary restriction on a natural commons). From the excluded migrant's seat, the constraint is pure extraction (rights violation with manufactured justification). From the incumbent citizen's seat, the constraint appears as protection but functions as misdirection (the real driver of their precarity is automation + globalization, not migration). The engine will compute these seats as reporting different constraint types: the agenda-setter may report rope (coordination with enforcement), while the payer seats report snare (extraction with suppression). This divergence is the measurement the corpus exists to capture — where the reading's own normative frame produces the deepest structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   From the freedom-primary reading's seat: excluded migrants and displaced workers are targets (d→1.0), bearing full extraction cost with no option to exit the constraint itself (trapped or identity_locked). State enforcement apparatus is beneficiary (d→0.0), collecting authority, labor control, and fees. Incumbent citizens sit near symmetric (d≈0.5) — they receive misdirection benefit (their grievance is reframed as invasion), but they also face material effects of low-wage labor scarcity and reduced economic growth. Nationalist political coalitions are beneficiaries (d→0.1), collecting electoral power. Human rights advocacy is excluded, not coordinated — their d is undefined (they are not seated in the constraint, only in the competing reading). The engine will compute different directionalities for each seat from the structural data (beneficiary/victim + power + exit); the reading declares the structural relationships that make this computation sensible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state security in a Westphalian system of competing sovereigns) is substantially solved: modern borders face negligible military threat; contemporary border enforcement targets economic migrants and refugees, not invading armies. Under the freedom-primary reading, the constraint has dead-founding-problem + world-rearranges signature — it persists not because the founding problem is live, but because the extracted value (labor control, political capital, enforcement authority) benefits the agenda-setter. The constraint is a mandatrophic snare: it evolved from a genuine security coordination (justified border control) into an extraction mechanism whose justification has become purely theatrical. The rising theater ratio (0.28→0.42) and rising extraction (0.68→0.82) while suppression stays high (0.81→0.88) is the pattern of mandatrophy: the constraint must intensify enforcement (higher suppression) because its justifying narrative is eroding (higher theater = more of the work is theatrical rather than functional). Classification prevents mislabeling this as rope (which would require the coordination function to be genuine and non-extractive); the snare classification acknowledges that the constraint extracts real value from the powerless and that the extraction persists through suppression, not through participant preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_movement_axiom_contestation,
    'What is the normative status of freedom of movement as an axiom grounding the reading''s claim? Is it genuinely foundational, or is it one constraint among others in a normative calculus that can be overridden?',
    'Philosophical analysis of the foundations of human rights law and the status of freedom of movement specifically. Testing the reading''s internal coherence: if freedom of movement is foundational, then any border exclusion requires extraordinary justification (the reading''s claim). If it is defeasible, the reading collapses toward qualified_sovereignty.',
    'If the axiom is foundational and non-defeasible, the reading is internally coherent and should produce a consistent snare classification across contexts. If it is defeasible, the reading loses its distinguishing force and becomes indistinguishable from qualified_sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_movement_axiom_contestation, conceptual, 'The normative status of freedom of movement as a foundational axiom.').

omega_variable(
    cosmopolitan_authority_institutionalization,
    'Can the freedom-primary reading achieve actual institutional authority over border policy, or is its function permanently oppositional (advocacy, critique, shadow jurisdiction)?',
    'Institutional history and future scenarios: has any modern state adopted a border regime where freedom of movement is the default and exclusion requires extraordinary, individually-justified permission? If not, under what conditions might it? Or is the reading''s institutional destiny to remain a critique of all existing regimes without ever governing?',
    'If the reading can institutionalize, the snare classification reflects actual future policy drift (toward mandatory individual-level justification for each exclusion). If it cannot institutionalize, the snare classification reflects the reading''s normative critique of an entrenched regime it cannot displace — the constraint persists not because the reading''s logic is weak, but because the reading has no institutional power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmopolitan_authority_institutionalization, empirical, 'Institutional viability of the freedom-primary reading as actual governing authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bord_tr_t5, border_normative_status__freedom_primary, theater_ratio, 5, 0.31).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__freedom_primary, theater_ratio, 10, 0.35).
narrative_ontology:measurement(bord_tr_t15, border_normative_status__freedom_primary, theater_ratio, 15, 0.38).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__freedom_primary, theater_ratio, 20, 0.41).
narrative_ontology:measurement(bord_tr_t25, border_normative_status__freedom_primary, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(bord_be_t5, border_normative_status__freedom_primary, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(bord_be_t10, border_normative_status__freedom_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(bord_be_t15, border_normative_status__freedom_primary, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(bord_be_t20, border_normative_status__freedom_primary, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(bord_be_t25, border_normative_status__freedom_primary, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.81).
narrative_ontology:measurement(bord_su_t5, border_normative_status__freedom_primary, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(bord_su_t10, border_normative_status__freedom_primary, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(bord_su_t15, border_normative_status__freedom_primary, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(bord_su_t20, border_normative_status__freedom_primary, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(bord_su_t25, border_normative_status__freedom_primary, suppression_requirement, 25, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.18).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, labor_mobility_constraint).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, refugee_protection_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading (freedom_primary) of a three-reading kernel (border_normative_status). The sibling constraints instantiate sovereignty_primary and qualified_sovereignty readings of the same kernel — same boundaries, different normative framings producing different beneficiary/victim structures, different ε values, and different classifications. All three readings compete for institutional authority over actual border policy. The network links capture the structural dependency: the freedom-primary reading's credibility depends partly on whether the other readings can be shown to be incoherent or empirically false; sovereignty_primary and qualified_sovereignty similarly depend on discrediting the freedom-primary axioms. This is a constraint family exhibiting reading-level contention within a shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
