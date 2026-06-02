% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary: Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The physical appropriation reading of the takings clause boundary
 *   establishes that government compensation obligations are triggered only
 *   by direct physical seizures or permanent physical occupations — not by
 *   regulations that reduce property value or eliminate economically viable
 *   use. This reading crystallizes a narrow victim set (only those whose
 *   property is physically taken) and a broad government regulatory
 *   exemption. The constraint exhibits the structural tension characteristic
 *   of tangled ropes: genuine coordination function (the government needs
 *   power to act for collective purposes like national defense, environmental
 *   protection, infrastructure) combined with asymmetric extraction (property
 *   owners bear regulatory losses without remedy, while government retains
 *   flexibility to regulate broadly). The theater ratio (0.48) reflects
 *   moderate performativity: the physical/regulatory boundary maintains
 *   semantic coherence and precedential clarity, yet its functional work has
 *   eroded as economic regulations impose losses equivalent to physical
 *   seizure. Over the measurement interval (80-year span from mid-20th
 *   century forward), the theater ratio has risen (0.35 → 0.52) as the gap
 *   between physical and regulatory harm has widened without doctrinal
 *   adjustment, making the boundary increasingly ritualistic. The reading is
 *   one pole of a fundamental dispute about the takings clause's scope —
 *   competing against regulatory takings and categorical takings readings
 *   that would expand the victim set and compensation obligation.
 *
 * KEY AGENTS:
 *   - Government Regulatory Authority: Primary beneficiary (institutional/arbitrage) — retains broad regulatory power without compensation liability; can act on public necessity without internal cost accounting
 *   - Property Owners Subject to Physical Seizure: Primary victims (powerless/trapped) — face direct dispossession with no meaningful remedy under this reading; extreme suppression
 *   - Organized Propertied Interests (Corporations, Wealthy Landowners): Secondary victims (powerful/constrained) — experience both coordination benefits (secure property framework) and extraction (targeted losses); can afford legal defense and political leverage
 *   - Middle-Class Property Owners: Moderate victims (moderate/constrained) — depend on coordination (property security for borrowing, family stability) but vulnerable to physical appropriation with limited recourse
 *   - Regulatory/Environmental Constituencies: Indirect beneficiaries (organized/constrained) — benefit from government's unencumbered power to regulate for public goods (environmental protection, land use control), though compensation obligations might redirect regulatory capability
 *   - Judicial System (Takings Clause Interpreter): Institutional custodian (institutional/arbitrage) — maintains the physical/regulatory boundary through precedent; derives authority from interpretive lineage back to founding constitutional text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.38).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.62).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '62c09d61-38cb-4732-ac90-cf077fd3edfe').
narrative_ontology:cs_kernel_codification('62c09d61-38cb-4732-ac90-cf077fd3edfe', fixed_text).
narrative_ontology:cs_authority_grounding('62c09d61-38cb-4732-ac90-cf077fd3edfe', lineage).
narrative_ontology:cs_interpretation_layer_present('62c09d61-38cb-4732-ac90-cf077fd3edfe').
narrative_ontology:cs_reading_relation('62c09d61-38cb-4732-ac90-cf077fd3edfe', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('62c09d61-38cb-4732-ac90-cf077fd3edfe', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('62c09d61-38cb-4732-ac90-cf077fd3edfe', foundational, physical_appropriation_triggering_compensation).
narrative_ontology:cs_axiom_status(physical_appropriation_triggering_compensation, holdable).
narrative_ontology:cs_axiom_grounding('62c09d61-38cb-4732-ac90-cf077fd3edfe', physical_appropriation_triggering_compensation, deontological).
narrative_ontology:cs_axiom('62c09d61-38cb-4732-ac90-cf077fd3edfe', foundational, regulatory_action_exempt_from_compensation).
narrative_ontology:cs_axiom_status(regulatory_action_exempt_from_compensation, holdable).
narrative_ontology:cs_axiom_grounding('62c09d61-38cb-4732-ac90-cf077fd3edfe', regulatory_action_exempt_from_compensation, instrumental).
narrative_ontology:cs_reference_frame('62c09d61-38cb-4732-ac90-cf077fd3edfe', physical_taking_requires_compensation).
narrative_ontology:cs_drift_state('62c09d61-38cb-4732-ac90-cf077fd3edfe', contemporary_regulatory_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62c09d61-38cb-4732-ac90-cf077fd3edfe', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulatory_authority).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_subject_to_physical_seizure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTY OWNER FACING PHYSICAL APPROPRIATION (SNARE) — Faces direct physical dispossession with no meaningful exit or compensation mechanism under this reading. Suppression is extreme: the constraint forecloses all traditional remedies (replacement in kind, fair market value, relocation support) and treats physical loss as an acceptable externality of regulatory action. The victim experiences maximum extraction: loss of property with no remedy, no negotiation, no exit path.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT REGULATORY AUTHORITY (ROPE) — Under this reading, the government experiences the constraint as enabling coordination: the ability to act physically (seize land for public roads, occupy property for military defense, remove improvements for environmental restoration) without compensation liability creates a tool for collective action that protects shared interests. The constraint functions as pure coordination of public necessity. Arbitrage exit means the government can calibrate when to invoke physical appropriation versus regulatory restriction; the classification remains Rope because the core function is genuine — public goods provision.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED PROPERTIED INTERESTS (TANGLED ROPE) — Wealthy landowners and corporations with institutional capacity can navigate the physical appropriation boundary through legal challenge, political leverage, and property insurance. They experience the constraint as both coordination (government's power to act secures the legal framework that enables their property holdings) and extraction (they bear disproportionate losses when physically targeted, relative to their ability to exit). Constrained exit because they can afford legal defense and lobbying but cannot completely avoid regulatory risk.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TAKINGS CLAUSE DOCTRINE (PITON) — The physical appropriation boundary is an entrenched doctrine that persists through institutional inertia despite eroding functional justification. The theater ratio is moderate (0.48) because the reading maintains semantic coherence — 'physical' is a clear category — but the functional distinction between physical seizure and catastrophic regulatory harm has weakened. Many regulations impose losses equivalent to physical appropriation (landmarking that reduces value 90%, zoning that eliminates use, environmental restrictions that render land worthless). The doctrine performs the ritualistic function of maintaining a boundary, not the practical function of protecting property.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational view, the physical/regulatory distinction maps onto a conceptually crisp boundary grounded in the nature of property itself: seizure is *taking*, regulation is *governing*. This perspective naturalizes the boundary as a logical necessity — a government cannot regulate while simultaneously compensating every loss, or regulation becomes impossible. However, this naturalization obscures the reading's status as a READING — a choice among defensible framings of the takings clause, not an inevitable derivation from property theory.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MIDDLE-CLASS PROPERTY OWNERS (TANGLED ROPE) — Small homeowners and business owners experience genuine coordination benefit (secure property rights framework enables borrowing, investment, family stability) but also bear significant extraction when physically targeted. Unlike organized interests, they lack legal resources to challenge appropriation; unlike the powerless, they have some assets to lose and some political voice. The constraint appears as moderate extraction within a coordinating framework — the system both protects and endangers their interests.
constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(takings_clause_boundary__physical_appropriation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, TR),
    TR >= 0.70.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading creates real extraction for property owners facing seizure (no compensation remedy, forced divestment, opportunity cost), but the extraction is constrained by the narrowness of the victim set — only those with property physically appropriated, not those bearing regulatory losses. Base extractiveness reflects the government's asymmetric benefit (uncompensated power to act) weighed against the limited scope of direct victims. Extractiveness has risen over the measurement interval (0.28 → 0.38) as regulatory alternatives have made physical seizure less common while regulatory restrictions have become the primary tool, shifting the constraint's functional relationship. Suppression (0.62): Moderate-high. Victims of physical appropriation face near-total suppression — they cannot negotiate compensation, cannot exit the regulatory regime, cannot recover losses through judicial remedy. Suppression is not 0.95 because the physical appropriation reading is itself an exit path for property owners who might otherwise fear catastrophic regulatory harm: the clear boundary creates predictability and insurable risk, which is a limited form of mobility (knowing the rule allows planning). The suppression measurement reflects the irreversibility of appropriation combined with the availability of procedural (though not substantive) remedy. Theater ratio (0.48): Moderate-high. The physical/regulatory distinction maintains formal clarity and semantic coherence — 'taking' is lexically distinct from 'regulating' — but the functional distinction has eroded. Zoning restrictions eliminating 95% of property value are economically equivalent to physical seizure; environmental regulations rendering land worthless are functionally appropriative; yet the doctrine classifies them as non-compensable regulation. The theater ratio measures the gap between the boundary's formal work (keeping categories separate) and its practical work (protecting property owners from equivalent losses). The rise from 0.35 to 0.52 over 80 years reflects regulatory expansion without doctrinal adjustment, increasing the constraint's performative character.
 *
 * PERSPECTIVAL GAP:
 *   The physical appropriation reading generates acute perspectival divergence across structural positions. The powerless property owner whose land is seized experiences pure extraction (Snare) — no benefit, no coordination value, only loss and suppression. The government experiences pure coordination (Rope) — the ability to act for collective purposes is the constraint's entire function from their vantage. Organized property interests experience tangled rope — they benefit from the secure property framework the constraint enables, but they also face targeted extraction if physically appropriated. The judicial system sees its own doctrine as institutionalized (Piton) — maintaining the boundary through precedent despite eroding functional justification. The analytical observer risks seeing natural law (Mountain) — 'taking' and 'regulating' as logically distinct categories — but the structural data reveals the boundary as a reading, a choice about where to draw the line, not an inevitable derivation. The mandate gap (mandatrophy) is central: which perspective's classification is correct? All are correct from their structural positions. The constraint exhibits the full range precisely because the reading is contestable — the boundary between physical and regulatory is not a fact of nature but a constructed institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: beneficiary status, victim status, and exit options determine how the constraint affects them. Government regulatory authority (beneficiary + arbitrage exit) derives d ≈ 0.05, experiencing negative effective extraction (the constraint subsidizes their action). Property owners subject to physical seizure (victim + trapped exit) derive d ≈ 0.95, experiencing maximum effective extraction. Organized property interests (mixed beneficiary/victim status + constrained exit) derive d ≈ 0.55, experiencing moderate extraction. The analytical observer (analytical context) derives canonical d ≈ 0.73, serving as the reference frame for mandatrophy analysis. No directionality overrides are needed — the standard derivation chain captures the structural relationships accurately. The perspectival gap in d values drives the gap in classifications: agents with high d (victims with trapped exit) classify as Snare; agents with low d (beneficiaries with arbitrage exit) classify as Rope; agents with intermediate d (mixed position + constrained exit) classify as Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading frame: the physical appropriation reading is ONE legitimate pole of a constitutional dispute, not the true classification that other readings must defer to. The mandatrophy question becomes: 'What reading of the takings clause is binding?' rather than 'What is the one true type?' Different interpretive communities (strict constructionalists viewing the clause as self-contained boundary; progressives viewing it as applying to severe regulatory harm; originalists viewing historical practice at ratification) hold different readings with equal structural coherence. The engine's false summit detector will identify the analytical observer's mountain classification as a naturalization of the reading — the boundary between physical and regulatory is not immutable law but institutional choice. The resolution recognizes that the constraint is functionally two constraints simultaneously: (1) a coordination mechanism enabling government action (Rope from the government's view), and (2) an extraction mechanism leaving property owners uncompensated (Snare from the victim's view). The mandatrophy is resolved not by choosing one type but by recognizing that the indexical classification system IS the resolution: the constraint has no single type, only a presheaf of types across different observation points. The physical appropriation reading is the reading that maximizes the Rope classification (government coordination benefit) while minimizing victim protection — it is the reading that most strongly favors the government authority's structural interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_regulatory_equivalence,
    'At what point does regulatory harm become economically and legally equivalent to physical appropriation, making the distinction a false categorical boundary?',
    'Empirical analysis of property value destruction: comparison of losses from physical seizure vs extreme zoning restrictions, environmental regulations, landmarking prohibitions across decades of case law and property transactions',
    'If equivalence threshold crossed: this reading''s axiom (physical_appropriation_triggering_compensation) becomes undermined; regulatory takings reading gains structural ground. If distinction remains sharp: physical appropriation reading retains coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_regulatory_equivalence, empirical, 'Whether regulatory harm becomes economically equivalent to physical taking').

omega_variable(
    kernel_constraint_framing_ambiguity,
    'Is the takings clause a kernel constraining legitimate government action, or a reading of a larger kernel about property rights that competes with regulatory readings?',
    'Constitutional historical analysis: examining whether Framers treated the takings clause as self-contained boundary (physical/not physical) or as one application of a broader principle that could extend to severe regulatory harm',
    'If kernel-constraining: this reading''s boundary is the referent; regulatory readings are false extensions. If reading of larger kernel: this reading is one pole of an ongoing dispute; authority_grounding is contestable rather than settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_constraint_framing_ambiguity, conceptual, 'Whether the takings clause is a self-contained kernel or a reading of a larger property-rights kernel').

omega_variable(
    compensation_practicability,
    'If the regulatory takings reading were operationalized (compensation for severe regulatory losses), would government capacity to regulate effectively collapse or remain intact?',
    'Institutional analysis: modeling of compensation obligation scope under different threshold proposals (50% loss, 75% loss, economic-use elimination); comparison with international takings regimes (EU property directives, Canadian regulatory takings jurisprudence) and their regulatory effectiveness',
    'If collapse: suppression value (0.62) understates the constraint''s force; government regulation requires the physical boundary to remain enforceable. If intact: suppression is overstated; the constraint is more about wealth distribution than regulatory necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_practicability, empirical, 'Whether compensation for severe regulatory losses would impair government regulatory capacity').

omega_variable(
    reading_interpretive_authority_stability,
    'What interpretive authority legitimates the physical/regulatory distinction as binding, and is that authority stable or eroding?',
    'Doctrinal analysis tracking Supreme Court takings jurisprudence from Penn Central to present; identification of shifts in framing, justifications offered, and concurrence/dissent patterns indicating competing reading coalitions',
    'If interpretive authority stable: this reading remains entrenched (piton perspective accurate). If eroding: the reading''s reference frame is drifting; sister readings gaining institutional ground (drift_state: authority_erosion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_interpretive_authority_stability, empirical, 'Stability of judicial interpretive authority for the physical/regulatory distinction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_phys_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(takings_phys_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(takings_phys_tr_t80, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 80, 0.52).

% Extraction over time
narrative_ontology:measurement(takings_phys_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(takings_phys_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(takings_phys_be_t80, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(takings_phys_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(takings_phys_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(takings_phys_su_t80, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% The takings clause boundary decomposes into three reading-constraints with different ε values and victim sets. The physical appropriation reading (this file) establishes ε=0.38 with victim set {property_owners_subject_to_physical_seizure}. The regulatory takings reading extends victim set to include property owners suffering severe regulatory losses, with ε≈0.55. The categorical takings reading treats certain actions as automatic takings triggers, with ε≈0.62. Each reading is a distinct constraint on the kernel TAKINGS_CLAUSE_BOUNDARY. They coexist as competing interpretive frameworks held by different judicial coalitions and constitutional scholars. The physical appropriation reading is downstream of the regulatory takings and categorical readings in the sense that adopting those readings would foreclose or substantially pressure this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
