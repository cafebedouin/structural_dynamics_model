% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Printing Press and Reformation Co-Constitution
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   The printing press and the Protestant Reformation co-evolved in a
 *   bidirectional causal loop. The press provided the coordination
 *   infrastructure (vernacular text reproduction, distribution networks,
 *   public debate amplification) that made continental-scale doctrinal
 *   challenge possible — a genuine rope function. Simultaneously, reformers
 *   captured this infrastructure, suppressed rival interpretations (Catholic,
 *   radical, spiritualist), and built confessional institutions that
 *   atrophied into pitons (mandatory catechesis, territorial churches,
 *   censorship machinery). The extraction (ε=0.28) comes from the interaction
 *   term: neither press alone nor reformers alone produce this level of
 *   coordinated extraction. The constraint is claimed as tangled_rope because
 *   it has both coordination (press as infrastructure) and asymmetric
 *   extraction (reformer capture of that infrastructure), requiring active
 *   enforcement (censorship, licensing, territorial enforcement).
 *
 * KEY AGENTS:
 *   - vernacular_literacy_networks: Primary beneficiary (moderate/constrained) — gained access to texts and public debate
 *   - emergent_public_sphere_participants: Primary beneficiary (organized/constrained) — gained participation in new discourse spaces
 *   - institutional_church_authority: Primary victim (institutional/constrained) — lost monopoly on doctrinal interpretation and text control
 *   - latin_literate_elite: Secondary victim (powerful/constrained) — lost status as sole mediators of textual authority
 *   - reformer_networks: Agenda setter (institutional/arbitrage) — captured press infrastructure, built confessional institutions
 *   - printer_publishers: Beneficiary/payer (organized/mobile) — profited from vernacular demand, navigated confessional censorship
 *   - historical_analyst: Observer (analytical/analytical) — sees full bidirectional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.28).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.32).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Printing Press and Reformation Co-Constitution").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history/technology/religious").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '3d771f99-0979-4f31-b43e-21e0ec64b0eb').
narrative_ontology:cs_kernel_codification('3d771f99-0979-4f31-b43e-21e0ec64b0eb', distributed).
narrative_ontology:cs_authority_grounding('3d771f99-0979-4f31-b43e-21e0ec64b0eb', practice).
narrative_ontology:cs_interpretation_layer_present('3d771f99-0979-4f31-b43e-21e0ec64b0eb').
narrative_ontology:cs_reading_relation('3d771f99-0979-4f31-b43e-21e0ec64b0eb', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d771f99-0979-4f31-b43e-21e0ec64b0eb', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('3d771f99-0979-4f31-b43e-21e0ec64b0eb', foundational, bidirectional_causality_technology_society).
narrative_ontology:cs_axiom_status(bidirectional_causality_technology_society, holdable).
narrative_ontology:cs_axiom_grounding('3d771f99-0979-4f31-b43e-21e0ec64b0eb', bidirectional_causality_technology_society, empirically_contingent).
narrative_ontology:cs_axiom('3d771f99-0979-4f31-b43e-21e0ec64b0eb', foundational, interaction_term_generates_extraction).
narrative_ontology:cs_axiom_status(interaction_term_generates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3d771f99-0979-4f31-b43e-21e0ec64b0eb', interaction_term_generates_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('3d771f99-0979-4f31-b43e-21e0ec64b0eb', pre_print_textual_monopoly).
narrative_ontology:cs_drift_state('3d771f99-0979-4f31-b43e-21e0ec64b0eb', post_confessionalization_1580, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d771f99-0979-4f31-b43e-21e0ec64b0eb', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_literacy_networks).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, emergent_public_sphere_participants).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, institutional_church_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, latin_literate_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printer_publishers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, printer_publishers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, bidirectional_causality_technology_society).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, co_constitution_of_media_and_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artisans, merchants, urban citizens who gained access to vernacular texts through print. They participate in new reading publics, share texts, and develop literacy practices outside clerical mediation. Their exit is constrained by the confessional territorial structure — moving between Lutheran, Reformed, Catholic zones changes which texts are available, but the press infrastructure itself remains accessible.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_literacy_networks, beneficiary,
    moderate, generational, constrained, continental).

% Participants in the new publics formed around printed pamphlets, broadsheets, and vernacular Bibles — including women in reading circles, artisan discussion groups, and urban disputation audiences. They gain voice in doctrinal and political debate. Their exit is constrained by confessional censorship and social pressure, but the public sphere itself persists across confessional boundaries.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, emergent_public_sphere_participants, beneficiary,
    organized, biographical, constrained, continental).

% The Roman Curia, episcopacy, and monastic orders that held monopoly on Latin textual production, doctrinal interpretation, and sacramental authority. The press breaks their control: vernacular Bibles bypass clerical mediation, pamphlets circulate critique faster than censorship can respond, and the Index of Prohibited Books is a reactive trap. They cannot exit the press ecosystem — they must engage it (Catholic Reformation presses, Tridentine catechisms) on terms set by the technology they once regulated.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, institutional_church_authority, payer,
    institutional, generational, constrained, global).

% Humanist scholars, clerical bureaucrats, and legal professionals whose status derived from Latin literacy and textual mediation. Vernacular print erodes their monopoly: laypeople read scripture directly, pamphlets bypass academic disputation, and the Republic of Letters reorients around vernacular correspondence. Some adapt (Erasmus, Melanchthon) by becoming print authors; others resist (Cochlaeus, Eck) but lose cultural centrality. Exit means retreating to shrinking Latin enclaves.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, latin_literate_elite, payer,
    powerful, biographical, constrained, continental).

% Luther, Calvin, Zwingli and their institutional successors (territorial churches, consistories, academies). They capture the press: Luther's German Bible, Calvin's Geneva press, the Wittenberg printing network. They build confessional institutions (catechesis, visitation, synods) that atrophied into pitons — mandatory structures maintained theatrically after the doctrinal emergency passed. They have arbitrage exit: they move between territories, negotiate with princes, and shift confessional alignments. They collect the extraction (tithes, censorship fees, institutional positions) while administering the constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformer_networks, agenda_setter,
    institutional, generational, arbitrage, continental).

% Commercial printers in Basel, Strasbourg, Geneva, Wittenberg, Antwerp, Venice. They profit enormously from vernacular demand (Bibles, pamphlets, catechisms) — beneficiary. But they navigate confessional censorship, licensing, and territorial bans — payer. Their exit is mobile: they relocate presses across borders (e.g., Geneva to Lausanne, Antwerp to Frankfurt), print for multiple confessions, and exploit jurisdictional gaps. They are not captured by any single confessional project.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printer_publishers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printer_publishers, payer).

% The analytical seat that sees the full bidirectional structure: press as coordination rope, reformers as piton capture, Church as suppressed monopoly, vernacular networks as beneficiaries. This seat does not participate in the constraint but models its operation across all other seats. Exit is analytical — changing frameworks, not territories.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solved the coordination problem of reliable, scalable vernacular text reproduction and distribution across political/linguistic boundaries — enabling strangers to read the same text, coordinate interpretation, and sustain publics without physical co-presence.
% TRANSFER_FUNCTION: The constraint moves interpretive authority and textual revenue from Latin-literate clerical elites and the Roman Curia to reformer networks (who capture institutional positions, tithes, censorship control) and printer-publishers (who capture commercial revenue), while vernacular publics gain participation access as a diffuse byproduct.
% ABSENT_VOICES: Radical reformers (Anabaptists, Spiritualists, anti-Trinitarians) who used the press but were suppressed by magisterial reformers; Catholic lay reform movements (devotio moderna, oratories) that sought vernacular access without doctrinal rupture; Jewish and Muslim communities in Europe whose textual practices were disrupted by the same press infrastructure but who had no seat in the confessional settlement.
% DISAPPEARANCE_RATIONALE: If the press–reformation coupling vanished overnight (no vernacular print infrastructure, no confessional capture of it), the sixteenth-century European religious landscape would be unrecognizable: no mass vernacular Bibles, no pamphlet wars, no confessionalization, no territorial churches as we know them. The Catholic Church might have pursued internal reform on a different timeline; literacy would have spread differently; the public sphere would have emerged from different roots. The world rearranges.
% FOUNDING_PROBLEM: The late medieval Church's monopoly on Latin textual production and doctrinal interpretation created a coordination failure: laypeople could not access scripture, dissent could not coordinate across distances, and reform movements remained local and suppressible. The press–reformation coupling was built to solve this by creating a vernacular text infrastructure that bypassed clerical mediation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (clerical textual monopoly blocking lay access) is dead: vernacular scripture is universal, literacy is widespread, and the press infrastructure is no longer contested by Church authority. Corroboration: Catholic historians (e.g., Jedin, O'Malley) acknowledge the Council of Trent accepted vernacular catechesis; Protestant historians (e.g., Ozment, MacCulloch) note confessional churches became new monopolies. No major historian argues the original textual scarcity problem persists. The arrangement persists as mandatrophy — confessional institutions maintain structures built for a solved problem.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.08 (pre-Reformation press as commercial rope) to 0.28 (post-1555 confessional press as tangled_rope) because reformer institutions layer extraction onto press coordination. Suppression rises from 0.12 to 0.32 as confessional states enforce orthodoxy. Theater ratio rises from 0.05 to 0.47 as catechesis/visitation become performative maintenance of confessional boundaries. The peak extraction at 1580 reflects confessionalization's mature enforcement. Accessibility collapse (0.48) and resistance (0.57) are moderate: alternatives (radical reformation, Catholic reform, spiritualism) persisted but were marginalized, not eliminated.
 *
 * PERSPECTIVAL GAP:
 *   The press-as-rope seat (vernacular networks, public sphere) experiences coordination: new capabilities, expanded participation. The reformer-as-piton seat (confessional institutions) experiences extraction: they capture the press's gains, suppress alternatives, and maintain atrophied structures theatrically. The Church authority seat experiences suppression: its monopoly is broken by the very infrastructure it once regulated. The engine computes these divergences from power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vernacular_literacy_networks, emergent_public_sphere_participants) get low d — the press subsidizes their participation. Victims (institutional_church_authority, latin_literate_elite) get high d — the constraint extracts their monopoly rents and status. Reformer_networks are agenda_setters with arbitrage exit — they administer the constraint and can move between confessional projects, so d is near beneficiary end but they also extract. Printer_publishers are dual-role: they benefit from demand (beneficiary) but pay censorship/compliance costs (payer) with mobile exit across territories. The interaction-term ε means directionality is not reducible to any single agent's position.
 *
 * MANDATROPHY ANALYSIS:
 *   The press's coordination function (vernacular text reproduction) remains live — the founding problem of textual scarcity is solved. The reformers' capture of that function for confessional enforcement is mandatrophy: the original problem (Church monopoly) is dead, but the extraction machinery (censorship, territorial churches) persists. This prevents mislabeling the press as pure extraction (it coordinates) or the reformers as pure coordination (they extract via piton). The tangled_rope classification captures both: the press is the rope, the confessional layer is the snare tangled in it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the technology_reformation_causality kernel, specifically the co_constitution_reading?',
    'Structural decomposition: this reading''s bidirectional causality and interaction-term extraction are mutually exclusive with the technological_determinism_reading''s inevitability claim and the beneficiary_agency_reading''s instrumental tool framing. The engine computes per-reading ε and classification independently.',
    'Confirms this is a kernel reading subject to Rule 1–4. The sibling readings instantiate separate constraints with their own ε, beneficiaries/victims, and types. Do not average or hedge across them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel/reading frame for this constraint story.').

omega_variable(
    interaction_term_extraction_source,
    'Does the extraction (ε=0.28) arise primarily from the technology–social interaction term rather than from either component alone?',
    'Counterfactual comparison: (a) press without reformers — vernacular print markets exist but no mass doctrinal challenge; (b) reformers without press — localized disputation, no continental contagion. If both counterfactuals show low extraction, the measured ε belongs to the coupling.',
    'If the interaction term is the source, this is a genuine tangled_rope where coordination (press) and extraction (reformer capture of press) are inseparable. If one component dominates, the constraint decomposes into two stories per ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_extraction_source, empirical, 'Whether extraction is generated by the technology–social coupling itself.').

omega_variable(
    reformer_piton_ambiguity,
    'Are the reformers'' atrophied alternatives a structural piton (coordination function degraded, maintained by inertia/theatricality) or an adaptive strategy (reformer networks actively suppress alternatives to maintain capture)?',
    'Trace post-1555 reformer institution-building: if they constructed new coercive structures (confessional churches, censorship, mandatory catechesis) that mirror the suppression they once opposed, the piton reading is theatrical adaptation. If they inherited structures and failed to dismantle them, it is inertial piton.',
    'Theatrical adaptation → reformers as agenda_setter with extractive capture (snare/tangled_rope drift). Inertial piton → reformers as legacy administrators of a degraded rope, lower effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformer_piton_ambiguity, conceptual, 'Whether reformer piton status is inertial or actively maintained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1580).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1480, technology_reformation_causality__co_constitution_reading, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1517, technology_reformation_causality__co_constitution_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.28).
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1555, technology_reformation_causality__co_constitution_reading, theater_ratio, 1555, 0.41).
narrative_ontology:measurement(tech_ref_causality_co_const_tr_t1580, technology_reformation_causality__co_constitution_reading, theater_ratio, 1580, 0.47).

% Extraction over time
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1480, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1480, 0.12).
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1517, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1517, 0.18).
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.24).
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1555, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1555, 0.28).
narrative_ontology:measurement(tech_ref_causality_co_const_be_t1580, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1580, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1450, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1450, 0.12).
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1480, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1480, 0.18).
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1517, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1530, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1530, 0.32).
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1555, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1555, 0.32).
narrative_ontology:measurement(tech_ref_causality_co_const_su_t1580, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1580, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.03).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the technology_reformation_causality kernel. This reading (co_constitution) claims ε from the technology–social interaction term. technological_determinism_reading claims ε from technology alone (inevitability narrative). beneficiary_agency_reading claims ε from reformer agency alone (instrumental tool narrative). All three have distinct ε, beneficiaries/victims, and types. Linked via affects_constraints for contamination propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, institutional, 0.15).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
