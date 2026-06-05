% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaty Substrate (Extinguishment Reading)
 *   domain: legal_anthropology/indigenous_law/constitutional_theory
 *
 * SUMMARY:
 *   The extinguishment reading of historical treaties frames Indigenous
 *   territorial cession as a completed property transaction: Indigenous
 *   nations permanently alienated sovereignty over ceded territories in
 *   exchange for defined reserves and annuity payments. This reading became
 *   the dominant legal doctrine in Canadian, Australian, and U.S. law during
 *   the 19th–20th centuries and remains institutionalized in property law,
 *   tax codes, and resource extraction frameworks. Under this reading,
 *   Indigenous nations possess no territorial jurisdiction outside reserve
 *   boundaries, cannot claim Aboriginal title to ceded territories, and
 *   retain only contractual rights (annuities, access) rather than sovereign
 *   claims. This constraint instantiates one reading of the
 *   historical_treaty_substrate kernel. It competes with two sibling
 *   readings: the stewardship_reading (cession meant temporary trusteeship,
 *   not permanent alienation) and the nation_to_nation_reading (treaties are
 *   international agreements between sovereigns, not property transactions,
 *   and do not extinguish Indigenous authority). The extinguishment reading
 *   is institutionalized across three state jurisdictions and embedded in
 *   property law, mining permits, hydro development, and land titles. Its
 *   enforcement has required sustained suppression — police/military power,
 *   court systems, administrative overhead — especially as Indigenous legal
 *   movements have mounted challenges. The measurement trajectory shows
 *   rising extractiveness and suppression as Indigenous jurisdictional claims
 *   have accumulated, indicating that maintaining the extinguishment doctrine
 *   requires increasing enforcement machinery.
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Victims (powerless/trapped) — permanently locked out of territorial jurisdiction and authority under extinguishment doctrine; residual treaty rights are narrow and state-contingent
 *   - Settler State Authority: Beneficiary (institutional/arbitrage) — captures full territorial authority and resource extraction rights; can arbitrage between different treaty interpretations
 *   - Settler Extractive Industries: Secondary beneficiary (powerful/arbitrage) — access to minerals, timber, hydropower on ceded territories; benefit from clarity of title and state enforcement
 *   - Indigenous Legal Movement: Organized resistance (organized/constrained) — challenge extinguishment reading through litigation and policy advocacy while constrained by burden-of-proof rules
 *   - Treaty Administration: Institutional maintainer (institutional/constrained) — maintains performative treaty framework through commemorations and annuity payments while enforcing underlying extinguishment doctrine
 *   - International Human Rights Framework: Analytical sunset (analytical/analytical) — UNDRIP, ILO 169, and transnational Indigenous advocacy construct alternative legitimacy substrate that undermines extinguishment authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.68).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.72).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaty Substrate (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '43807965-63d7-4ad3-a9b7-f6f80e8f1ebd').
narrative_ontology:cs_kernel_codification('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', fixed_text).
narrative_ontology:cs_authority_grounding('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', extraction).
narrative_ontology:cs_interpretation_layer_present('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd').
narrative_ontology:cs_reading_relation('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', foundational, permanent_territorial_alienation_via_treaty_cession).
narrative_ontology:cs_axiom_status(permanent_territorial_alienation_via_treaty_cession, holdable).
narrative_ontology:cs_axiom_grounding('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', permanent_territorial_alienation_via_treaty_cession, conventional).
narrative_ontology:cs_axiom('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', foundational, property_transaction_model_governs_indigenous_cession).
narrative_ontology:cs_axiom_status(property_transaction_model_governs_indigenous_cession, holdable).
narrative_ontology:cs_axiom_grounding('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', property_transaction_model_governs_indigenous_cession, instrumental).
narrative_ontology:cs_reference_frame('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', permanent_territorial_alienation_framework).
narrative_ontology:cs_drift_state('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', contemporary_indigenous_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('43807965-63d7-4ad3-a9b7-f6f80e8f1ebd', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_authority).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_extractive_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_territorial_jurisdiction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS NATION (SNARE) — Locked into a reading where territorial sovereignty has been permanently alienated. Exit from this constraint would require abandoning the entire legal framework that defines treaty relationship. Suppression is high: the treaty is enforced by state apparatus, backed by police/military power. Extraction is severe: territorial jurisdiction has been extinguished; residual 'rights' are narrow and contingent on state recognition. No escape route within the current legal regime.
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SETTLER STATE AUTHORITY (ROPE) — Experiences the treaty as a pure coordination mechanism: establishing territorial authority, clarifying resource rights, enabling settler expansion without continuous military conflict. The state can arbitrage between different interpretations of the treaty to shift extraction flows. Net beneficiary. Experiences the constraint as legitimate property transaction that solved a collective action problem (how to allocate ceded territories).
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTEMPORARY TREATY ADMINISTRATION (PITON) — Maintains the treaty framework through administrative ritual despite recognition that the extinguishment premise has been substantially challenged by Indigenous legal movements, international human rights law, and empirical anthropology. Theater is moderate (0.58): treaty commemorations, annual payments, ceremonial acknowledgment of 'historical' relationship. But the functional performance (extinguishment doctrine) has degraded — enforcing it against Indigenous sovereignty claims requires continuous legal/administrative overhead. The system persists through institutional inertia.
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INDIGENOUS LEGAL MOVEMENT (TANGLED ROPE) — Organized Indigenous actors work to reinterpret or overturn the extinguishment reading while remaining bound by the same treaty text. They benefit from coordinated advocacy (litigation, policy reform) and generate genuine coordination value (clarifying what the treaty actually said). But they also experience extraction: the legal burden of proof falls on them to establish that extinguishment was not valid. Active enforcement (court systems, legislative gatekeeping) suppresses their exit options.
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS FRAMEWORK (SCAFFOLD) — UN Declaration on Rights of Indigenous Peoples (UNDRIP), ILO 169, and transnational Indigenous movements are constructing an alternative legal substrate that supersedes the extinguishment reading. This is a sunset perspective: UNDRIP has no enforcement mechanism but creates legitimacy pressure. The extinguishment reading loses authority as signatories align with international norms. Estimated sunset: 2035–2050 as domestic law realigns with UNDRIP norms. Low theater (written instruments), clear beneficiaries (Indigenous nations), clear sunset clause.
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGAL REALISM VIEW (MOUNTAIN) — From a civilizational/universal perspective, once territorial sovereignty is ceded via treaty and settler settlement occurs, the original Indigenous claim is structurally extinguished — reversion would require undoing centuries of property law, settlement patterns, and state infrastructure. This perspective sees extinguishment as an immutable fact of colonial history, not a contingent legal doctrine. However, this classification is vulnerable to false summit detection: the 'immutability' is actually enforced through state power and legal doctrine, not by logical necessity.
constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(historical_treaty_substrate__extinguishment_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, TR),
    TR >= 0.70.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The extinguishment reading is severely extractive from Indigenous nations' perspective: territorial authority is permanently alienated; reserves are typically 1–5% of ceded territories; annuities have eroded through inflation; resource wealth generated from ceded territories accrues entirely to settler state and extractive industries. The extraction is not total because some treaty rights (hunting, harvesting, annuities) remain and are legally recognized, preventing the classification from reaching pure taking. The 0.68 value reflects a snare-range extraction with residual coordination (reserves, annuities) that prevents total dispossession. Suppression (0.72): Very high. The extinguishment doctrine is enforced through state police/military power, court system, administrative enforcement (permits, licenses, police action against jurisdictional claims), and legislative authority. Indigenous nations cannot exit this constraint within the current legal regime without either abandoning the treaty relationship or overturning the entire property law framework. Suppression has risen from 0.55 to 0.72 across the 100-year interval as Indigenous legal challenges have mounted, requiring increased enforcement overhead. Theater ratio (0.58): Moderate. The constraint combines functional enforcement (police/courts/permits) with performative elements (treaty commemorations, annuity ceremonies, formal acknowledgment of 'historical' relationship). The performative content has increased as the underlying functional doctrines have been challenged — creating ceremonial recognition without shifting jurisdictional authority. Theater rising from 0.35 to 0.58 indicates increasing theatrical maintenance relative to reduced legitimacy of the extinguishment premise.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why the same treaty text produces radically different classifications from different structural positions. The Indigenous nation locked into the extinguishment reading sees a snare — permanent extraction, high suppression, no exit. The settler state sees a rope — a coordination mechanism that enabled peaceful expansion and clarified property rights. The Indigenous legal movement sees tangled rope — they coordinate through litigation and advocacy while experiencing extraction (burden of proof, limited legal standing). The international human rights framework sees scaffold — an emerging alternative legal substrate with a sunset clause as UNDRIP norms mature. The settler state's treaty administration sees piton — the ritual persists through inertia while the underlying extinguishment premise degrades under challenge. The legal realist sees mountain — extinguishment as an immutable fact of colonial history. The perspectival range is unusually wide because the constraint operates at multiple institutional levels simultaneously and the reading status is explicitly contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position and exit options. Indigenous nations as victims with trapped exit options experience maximum d (approaching 1.0), producing maximum f(d) and thus high χ even with moderate ε. The settler state as beneficiary with arbitrage exit options experiences minimum d (near 0.0), producing negative f(d) — the constraint subsidizes this agent rather than extracting from them. Organized Indigenous movements have higher d than powerless individuals but lower than those with no organizational capacity, reflecting their constrained exit options and partial agency. The analytical observer at the international/universal scope experiences medium d (0.72), producing moderate f(d) reflecting the observer's position outside the constraint but within its political field.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the classification (snare) depends on the reading choice. Under the extinguishment reading, Indigenous nations are trapped in a snare: territorial jurisdiction is permanently lost, extraction is severe, suppression is high. Under the stewardship reading, the same treaty text produces tangled_rope or rope from the Indigenous perspective (temporary suspension, reversible if settler state violates terms). Under the nation_to_nation reading, the same text produces mountain (treaties are contracts between sovereigns, not property transactions, so they cannot extinguish sovereignty). The mandatrophy is not about which type is correct but about which reading of the kernel you adopt. The engine's role is to show that multiple readings are coherent instantiations of different normative axioms, and that choosing among them is a committer decision, not a facts-on-the-ground discovery. The measurement trajectory (rising suppression and extractiveness) shows that maintaining the extinguishment reading requires increasing enforcement machinery, suggesting that the reading's legitimacy is eroding and may become unsustainable without further escalation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_vs_suspension_ambiguity,
    'Was sovereignty extinguished permanently via treaty cession, or merely suspended pending satisfaction of treaty terms and Indigenous consent?',
    'Textual analysis of original treaty language; comparison across colonial jurisdictions and their treaty frameworks; Indigenous oral history records; contemporary reinterpretation litigation',
    'If permanent extinguishment: snare classification holds; Indigenous nations are trapped. If suspension: classification shifts to tangled_rope or rope depending on remediation pathways. If neither: reframes entire relationship as coercive occupation lacking legal foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinguishment_vs_suspension_ambiguity, conceptual, 'Whether treaty cession extinguished or suspended Indigenous sovereignty').

omega_variable(
    consideration_adequacy_ambiguity,
    'Did the reserves and annuities constitute adequate consideration for permanent territorial cession, or were they materially inadequate and thus invalidate the extinguishment premise?',
    'Comparative value analysis: land area ceded vs reserve land granted; historical annuity purchasing power vs land economic value; land development patterns post-treaty; contemporary resource values (minerals, timber, hydro)',
    'If consideration adequate: snare classification consistent with doctrine of binding property transfer. If inadequate: snare classification becomes indefensible; extinguishment was extraction without exchange, potentially reversible via restitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consideration_adequacy_ambiguity, empirical, 'Whether treaty consideration was adequate for permanent cession').

omega_variable(
    informed_consent_doctrine_shift,
    'Can a treaty signed under coercive conditions and without full understanding of its permanent character constitute binding consent to extinguishment?',
    'Reconstruction of bargaining conditions; analysis of translation accuracy; evidence of explicit discussion of ''permanence'' or ''forever'' language; comparison with contemporary consent standards in property law',
    'If no informed consent: extinguishment doctrine collapses to coercive taking. If consent doctrine applies: snare classification may shift to tangled_rope (asymmetric but consensual extraction). If doctrine inapplicable to colonial context: reframes as occupation without legal foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_doctrine_shift, conceptual, 'Whether treaty consent was informed and voluntary').

omega_variable(
    interpretation_reading_contingency,
    'Is the extinguishment reading one legitimate interpretation among others (coexists_with sibling readings), or does it logically foreclose alternative readings?',
    'Close reading of treaty text; examination of settler state''s own jurisprudence recognizing Indigenous rights; analysis of whether extinguishment can hold alongside recognition of Aboriginal title or Aboriginal rights',
    'If coexists_with: kernel reading frame applies; extinguishment is a committer choice, not a logical fact. If forecloses: only one reading can be held within a single legal framework; the competition is zero-sum. If neither: reframes the kernel itself as incoherent or unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_reading_contingency, conceptual, 'Logical status of extinguishment reading relative to sibling readings').

omega_variable(
    perpetual_enforcement_cost,
    'How much state apparatus and enforcement capacity is required to maintain the extinguishment reading against ongoing Indigenous jurisdictional claims?',
    'Analysis of litigation burden (land claims, water rights, resource extraction disputes); police/military resource allocation to enforce state authority on traditional territories; cost of administrative overhead; comparison with enforcement cost for alternative readings',
    'If enforcement cost is rising: extinguishment reading is destabilizing and may shift toward piton classification (theater rising, suppression rising). If stable: snare classification holds. If enforcement cost exceeds benefit: reading may become economically indefensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_enforcement_cost, empirical, 'Sustained enforcement cost for extinguishment doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_ext_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(treaty_ext_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(treaty_ext_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(treaty_ext_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(treaty_ext_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(treaty_ext_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(treaty_ext_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(treaty_ext_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(treaty_ext_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, identity_coordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, aboriginal_title_doctrine).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indian_act_paternalism).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, resource_extraction_legitimacy).

% DUAL FORMULATION NOTE:
% This story is one reading of the historical_treaty_substrate kernel. The stewardship_reading and nation_to_nation_reading are sibling constraint stories covering the same treaty text under different normative interpretations. They share the same empirical domain but have structurally distinct ε values, victim sets, and classifications. All three are linked via network edges to show their interdependence. Sibling stories must be authored separately per ε-invariance principle; this file covers only the extinguishment reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
