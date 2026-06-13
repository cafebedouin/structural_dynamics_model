% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Treaty of Waitangi: Crown Sovereignty Reading (English Article I)
 *   domain: constitutional/post-colonial/indigenous-rights
 *
 * SUMMARY:
 *   The Crown Sovereignty Reading of the Treaty of Waitangi interprets
 *   English Article I as a cession of complete sovereignty to the British
 *   Crown, establishing Westminster parliamentary supremacy over New Zealand
 *   territory without requirement for Māori consent on legislation or
 *   resource allocation. This reading justifies unilateral Crown authority to
 *   alienate Māori lands, impose legislation without Māori participation, and
 *   subordinate Māori interests to parliamentary will. It is contested by two
 *   sibling readings: the Partnership Reading (Treaty established Crown-Māori
 *   co-governance requiring good faith consultation) and the Rangatiratanga
 *   Reading (Māori text Article II retained tino rangatiratanga — full
 *   authority — over lands and resources, with Crown limited to kāwanatanga
 *   over settlers). This story instantiates ONLY the Crown Sovereignty
 *   Reading as a structurally coherent constraint with stable ε, independent
 *   of the sibling readings' ε values.
 *
 * KEY AGENTS:
 *   - Crown Parliament: institutional agenda-setter, exercises plenary legislative authority, interprets the constraint through statute and precedent
 *   - Māori iwi collectives: powerless victims, structurally excluded from sovereign decision-making, bear the costs of unilateral resource alienation and legislative imposition
 *   - European settlers: institutional beneficiaries, receive Crown protection of property rights and resource access secured by parliamentary sovereignty
 *   - Treaty Settlement Bodies: institutional observers, operate under statutory frameworks that presuppose Crown sovereignty as baseline
 *   - International observers: analytical excluded voices, critique the reading as incompatible with indigenous rights norms but carry no binding force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.78).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.81).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Treaty of Waitangi: Crown Sovereignty Reading (English Article I)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/post-colonial/indigenous-rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '2d8bda22-42f6-4d3c-9f34-5433bf2caff9').
narrative_ontology:cs_kernel_codification('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', fixed_text).
narrative_ontology:cs_authority_grounding('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', lineage).
narrative_ontology:cs_interpretation_layer_present('2d8bda22-42f6-4d3c-9f34-5433bf2caff9').
narrative_ontology:cs_reading_relation('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', foundational, crown_holds_plenary_sovereignty).
narrative_ontology:cs_axiom_status(crown_holds_plenary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', crown_holds_plenary_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', secondary, maori_subordination_to_parliamentary_will).
narrative_ontology:cs_axiom_status(maori_subordination_to_parliamentary_will, holdable).
narrative_ontology:cs_axiom_grounding('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', maori_subordination_to_parliamentary_will, conventional).
narrative_ontology:cs_reference_frame('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', westminster_parliamentary_sovereignty_established_1840).
narrative_ontology:cs_drift_state('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d8bda22-42f6-4d3c-9f34-5433bf2caff9', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, european_settlers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_collectives).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores high on extractiveness (0.78) because the Crown unilaterally controls the allocation of lands, resources, and legislative authority without Māori consent mechanisms or veto rights — this is pure asymmetric transfer from Māori to Crown/settlers. Suppression is high (0.81) because the constraint's persistence depends on active enforcement through legislation, judicial interpretation, and administrative practice; Māori alternative framings are excluded from sovereign decision-making. Theater ratio is moderate-high (0.42) because the constraint increasingly relies on performative remediation: Treaty settlement processes, consultation frameworks (Foreshore and Seabed Act amendments, co-management agreements), and parliamentary apologies create the appearance of redress while the underlying sovereignty claim persists unchanged. Accessibility collapse is moderate-high (0.72) because under this reading, once Māori accept the legitimacy of Crown sovereignty, no alternative path to resource control or legislative voice is available — exit is impossible (trapped, identity-locked). Resistance is high (0.68) because Māori iwi have mounted sustained, organized resistance through litigation (Ngāi Tahu v. Crown, Waitangi Tribunal findings), activism, and constitutional advocacy for 186 years. The measurement series track the constraint's evolution from initial imposition (low extractiveness at 1840, as the reading was fresh and contested) through consolidation (extractiveness rises to 0.62 by 1880 as land alienation accelerated), institutional entrenchment (0.71 by 1920 as statutory frameworks solidified), and increasing performativity (theater ratio rises from 0.08 to 0.42 as settlement and consultation regimes proliferate without changing the underlying sovereignty structure).
 *
 * PERSPECTIVAL GAP:
 *   Crown Parliament sits at the beneficiary end of directionality (d near 0.0): the constraint subsidizes Crown authority, resource access, and revenue. Māori iwi sit at the target end (d near 1.0): the constraint extracts sovereignty, lands, and resource authority from them with no exit. European settlers sit at moderate-beneficiary (d ~0.2): they benefit from Crown protection but are not the agenda-setter and bear some diffuse legislative costs. Treaty Settlement Bodies sit near symmetric (d ~0.5): they both administer the constraint (partial agenda-setter role) and are constrained by its sovereignty baseline. The engine should compute substantial seat divergence from this structural data: from Crown's position the arrangement is legitimate sovereignty and beneficial partnership; from Māori iwi's position it is extractive colonization; from settlers' position it is settled security; from international observers' position it is human-rights violation. This is not a failure of the constraint's description — this divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown derives directionality from its role as agenda-setter and primary beneficiary (collects sovereignty, resource authority, legislative supremacy) combined with its arbitrage exit options (can restructure the constraint through legislation at will). Directionality: low (beneficiary). Māori iwi derive directionality from their role as payers (bear the cost of alienated lands, excluded decision-making, imposed legislation) combined with their trapped exit (cannot physically leave, identity-locked to ancestral lands and governance responsibilities, no legal pathway to alternatives under this reading). Directionality: high (target). European settlers derive directionality from their role as beneficiaries (receive Crown protection, land access, resource rights) combined with their arbitrage exit (can migrate, can participate in democratic process, can coordinate with Crown interests). Directionality: low-to-moderate (beneficiary, but not the primary beneficiary or agenda-setter). No directionality overrides are needed; the automatic derivation from beneficiary/victim + exit captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding problem (post-1840 settler colonization created competing claims) was legitimately solved by asserting Crown sovereignty and subordinating Māori claims through the Courts and legislatures. But the founding problem status is now dead — Māori organizations, international law, and domestic constitutional scholars attest that the problem was never 'need for Crown sovereignty' but 'how to structure a bicultural state compatible with Treaty commitment.' The Crown reading forecloses genuine partnership and co-governance, making the founding mandate obsolete while the constraint persists through institutional inertia (statute law, judicial precedent, administrative practice) and performative remediation (settlement processes, consultation, apologies that do not change the underlying sovereignty structure). Theater ratio rising from 0.08 to 0.42 models this: early imposition required little performance; contemporary operation increasingly relies on settlement theater and consultation performance to maintain suppression without substantive power-sharing. The constraint is classifiable as Piton (atrophied coordination function, persistence through institutional inertia and performance) or Tangled Rope (genuine historical coordination function — resolving competing 1840s claims — but now purely extractive, requiring active enforcement to maintain suppression of Māori alternatives). The story authors it as Tangled Rope to capture the enforcement asymmetry; Piton classification would be supported if the constraint had genuinely become purely theatrical. Given the coercion grid shows sustained high suppression (0.81 at 2026), active enforcement remains substantial — the reading still functions as extractive coordination, not yet purely theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    english_vs_maori_text_divergence,
    'Does the English Article I text (''cede to Her Majesty'') represent the substantive agreement, or does the Māori Article II text (which uses ''kāwanatanga'' not ''mana'' or ''rangatiratanga'' for Crown authority) capture a genuine alternative commitment that was mistranslated or deliberately obscured?',
    'Linguistic analysis of 1840 Māori language usage comparing kāwanatanga, mana, and rangatiratanga in other contemporary documents; examination of rangatira testimony about what they understood themselves to be signing; comparison with other treaty texts of the era (e.g., US treaties) showing precedent for dual-text readings.',
    'If the Māori text represents a substantially different agreement (kāwanatanga as limited governorship over settlers, not plenary sovereignty), the Crown Sovereignty Reading collapses and the constraint''s ε shifts from 0.78 to near-zero (rangatiratanga reading takes precedence). If the texts are reconcilable, the sovereignty reading persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(english_vs_maori_text_divergence, empirical, 'Whether textual divergence reflects genuine alternative commitments or translation artifact.').

omega_variable(
    parliamentary_supremacy_legitimacy,
    'Is Westminster parliamentary supremacy (as exercised over Māori without their consent) a natural law of good governance, or a contingent institutional choice made by the Crown that could be legitimately replaced by co-governance structures?',
    'Comparative constitutional analysis: do other post-colonial democracies (Canada, Australia, New Zealand post-2010s reform proposals) operate with parliamentary supremacy over indigenous peoples, or have they adopted co-governance, veto rights, or tiered sovereignty models? If co-governance is viable and practiced elsewhere, the ''necessity'' of Crown unilateral supremacy is contingent, not natural.',
    'If parliamentary supremacy is contingent, the Crown Reading is a choice (not inevitable), and alternative readings become structurally equivalent rather than subordinate. If supremacy is natural/necessary, the reading is grounded in governance law rather than colonial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_supremacy_legitimacy, conceptual, 'Whether Crown parliamentary supremacy is legitimately universal or a contingent post-colonial choice.').

omega_variable(
    suppression_internalization_dynamics,
    'Is Māori acceptance of Crown sovereignty under the current regime structural (enforced by legal barriers, economic dependency, lack of institutional alternatives) or internalized (Māori have incorporated the legitimacy of Crown authority into their own identity and governance frameworks)?',
    'Trajectory analysis post-devolution: if substantive co-governance power were formally transferred to Māori iwi (as proposed in partnership and rangatiratanga readings), would suppression persist? If Māori institutions exercise genuine authority and Māori communities maintain confidence in iwi-led governance, suppression was primarily structural. If Māori defer to Crown even with formal co-governance power, suppression is internalized.',
    'If suppression is structural, removing the Crown Sovereignty Reading removes the suppression. If suppression is internalized (identity-locked, institutional dependence on Crown legitimacy), the constraint''s effective suppression persists even after formal authority shifts — the measured suppression is artificially low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamics, empirical, 'Whether measured suppression is structural or internalized through colonial institutional capture.').

omega_variable(
    theater_ratio_remediation_authenticity,
    'Do Treaty settlement processes, consultation frameworks, and co-management agreements (driving theater ratio from 0.08 to 0.42) represent genuine institutional evolution toward co-governance, or theatrical performance masking unchanging Crown supremacy?',
    'Audit of settlement outcomes: what percentage of signed settlements result in substantive shift of resource control, veto rights, or legislative input to Māori? What is the track record of Crown honoring consultation recommendations? Do consultation bodies have binding authority or advisory-only status? Post-settlement, do Māori hold substantive co-governance power or symbolic representation?',
    'If settlements and consultation shift genuine authority, the theater ratio is misleading and suppression should be measured as declining. If settlements preserve Crown supremacy while creating the appearance of partnership, the theater ratio is accurate and suppression remains high under the crown reading (declining only if the partnership reading becomes operative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_remediation_authenticity, empirical, 'Whether remediation frameworks represent institutional evolution or theatrical performance masking unchanged Crown supremacy.').

omega_variable(
    kernel_reading_foreclosure,
    'Is this Crown Sovereignty Reading logically foreclosed by the Rangatiratanga Reading (i.e., if Māori Article II established tino rangatiratanga over lands/resources, the Crown cannot simultaneously hold complete sovereignty), or do the readings merely coexist as competing interpretations that courts and political actors must choose between?',
    'Constitutional logic analysis: examine whether accepting both readings within a single legal framework creates internal contradiction or merely institutional choice. Can a legal system hold that Crown has complete sovereignty AND Māori iwi retain full authority over their lands simultaneously? If yes, coexist_with; if the readings directly contradict, forecloses.',
    'If foreclosed, the Crown Reading cannot persist indefinitely against the Rangatiratanga Reading — one must eventually prevail as institutional practice shifts. If coexist, both remain live options for different jurisdictions, political eras, or institutional actors indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether Crown Sovereignty and Rangatiratanga readings are logically incompatible or merely institutionally competing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1880, 0.12).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(wait_tr_t2026, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.45).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1880, 0.62).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1920, 0.71).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement(wait_be_t2026, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.55).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1880, 0.68).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1920, 0.74).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(wait_su_t2026, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2026, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi sovereignty allocation is a contested kernel instantiated in three structurally distinct constraint stories. This constraint (crown_sovereignty_reading) claims high extractiveness (ε=0.78) with Westminster supremacy and Māori subordination. The partnership_reading (sister constraint) claims moderate coordination (ε~0.45) with consultation obligations and co-governance framing. The rangatiratanga_reading (sister constraint) claims near-zero extraction for Māori (ε~0.15-0.25) with distributed authority. These are NOT alternative measurements of one constraint — they have different ε values, beneficiary/victim sets, and axioms. The readings coexist as live positions in New Zealand constitutional debate, linked via network edges: this reading COEXISTS_WITH partnership and rangatiratanga readings (all held by different institutional actors simultaneously). The network edges enable contamination analysis across the kernel family and support cross-reading comparison of how the same treaty text produces different constraints depending on reading chosen.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, institutional, 0.05).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, powerless, 0.95).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
