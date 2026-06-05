% ============================================================================
% CONSTRAINT STORY: crisis_machinery__sulla_constitutional_reaction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crisis_machinery__sulla_constitutional_reaction, []).

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
 *   constraint_id: crisis_machinery__sulla_constitutional_reaction
 *   human_readable: Sulla's Constitutional Dictatorship: Emergency Authority as Permanent Extraction
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   Sulla's dictatorship (82–79 BCE) represents a critical inversion of the
 *   emergency authority form. The classical Roman dictatorship — exemplified
 *   by Cincinnatus — was a temporary office: total power granted for a named
 *   task, deliberately brief (six months maximum by tradition), and designed
 *   to be laid down once the emergency was resolved. Sulla inverted this
 *   entirely. Granted a dictatorship 'to write the constitution and restore
 *   the state,' Sulla used the office to rebuild the state *on behalf of an
 *   oligarchic settlement* that benefited him and his supporters. The
 *   mechanism was proscription — published death lists that legalized the
 *   murder of political opponents and the confiscation of their property. The
 *   constraint exhibits all the structural hallmarks of a snare: indefinite
 *   authority (the office was never laid down), suppression via formalized
 *   execution lists (the proscription), extraction via confiscation (property
 *   transferred to Sullan beneficiaries), and minimal coordination benefit
 *   outside the beneficiary class. The theater of constitutionalism (the
 *   Senate delegated authority, formal procedures were followed) masked the
 *   actual mechanism (permanent oligarchic rule financed by confiscation).
 *   This reading is one interpretation of the contested kernel
 *   'crisis_machinery' — how emergency authority can be used and abused. It
 *   coexists with other readings: the term-limited dictatorship (Cincinnatus
 *   model) and the senatus consultum ultimum (blank-check decree). The key
 *   structural difference is that Sulla's dictatorship inverted the form — it
 *   was supposed to be an exception that restored normal governance, but it
 *   became the mechanism for constructing a new permanent settlement.
 *
 * KEY AGENTS:
 *   - Sulla: The dictator (institutional/arbitrary) — benefits from indefinite authority and property confiscation; controls the proscription machinery
 *   - Sullan oligarchs and land-buyers: Primary beneficiaries (powerful/arbitrage) — purchase confiscated estates at auction; gain property and political stability
 *   - The proscribed: Primary victims (powerless/trapped) — named on death lists, executed, property confiscated; no exit available within the dictatorship frame
 *   - Heirs of the dispossessed: Secondary victims (moderate/trapped generationally) — locked out of ancestral property by the settled Sullan distribution; cannot recover without overturning the settlement
 *   - The Senate: Institutional actor (institutional/constrained) — formally delegated authority but constrained by Sulla's military force; perceives the dictatorship as temporary constitutional reform
 *   - Republican institutional forms: Degraded structure (institutional/constrained) — persist but are hollowed out; real authority is concentrated in the dictator
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crisis_machinery__sulla_constitutional_reaction, 0.68).
domain_priors:suppression_score(crisis_machinery__sulla_constitutional_reaction, 0.82).
domain_priors:theater_ratio(crisis_machinery__sulla_constitutional_reaction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crisis_machinery__sulla_constitutional_reaction, extractiveness, 0.68).
narrative_ontology:constraint_metric(crisis_machinery__sulla_constitutional_reaction, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(crisis_machinery__sulla_constitutional_reaction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crisis_machinery__sulla_constitutional_reaction, snare).
narrative_ontology:human_readable(crisis_machinery__sulla_constitutional_reaction, "Sulla's Constitutional Dictatorship: Emergency Authority as Permanent Extraction").
narrative_ontology:topic_domain(crisis_machinery__sulla_constitutional_reaction, "legal/constitutional/political").

domain_priors:requires_active_enforcement(crisis_machinery__sulla_constitutional_reaction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(crisis_machinery__sulla_constitutional_reaction, '9d7b1641-f3e4-44c5-beab-723e247fc722').
narrative_ontology:cs_kernel_codification('9d7b1641-f3e4-44c5-beab-723e247fc722', formalized).
narrative_ontology:cs_authority_grounding('9d7b1641-f3e4-44c5-beab-723e247fc722', extraction).
narrative_ontology:cs_interpretation_layer_present('9d7b1641-f3e4-44c5-beab-723e247fc722').
narrative_ontology:cs_reading_relation('9d7b1641-f3e4-44c5-beab-723e247fc722', crisis_machinery__dictatorship_term_limited, forecloses).
narrative_ontology:cs_reading_relation('9d7b1641-f3e4-44c5-beab-723e247fc722', crisis_machinery__senatus_consultum_ultimum, influences).
narrative_ontology:cs_axiom('9d7b1641-f3e4-44c5-beab-723e247fc722', foundational, indefinite_emergency_authority_enables_extraction).
narrative_ontology:cs_axiom_status(indefinite_emergency_authority_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('9d7b1641-f3e4-44c5-beab-723e247fc722', indefinite_emergency_authority_enables_extraction, empirically_contingent).
narrative_ontology:cs_axiom('9d7b1641-f3e4-44c5-beab-723e247fc722', foundational, form_inversion_through_suppression).
narrative_ontology:cs_axiom_status(form_inversion_through_suppression, holdable).
narrative_ontology:cs_axiom_grounding('9d7b1641-f3e4-44c5-beab-723e247fc722', form_inversion_through_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('9d7b1641-f3e4-44c5-beab-723e247fc722', temporary_dictatorship_for_restoration).
narrative_ontology:cs_drift_state('9d7b1641-f3e4-44c5-beab-723e247fc722', indefinite_tenure_with_proscription, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('9d7b1641-f3e4-44c5-beab-723e247fc722', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(crisis_machinery__sulla_constitutional_reaction, crisis_machinery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crisis_machinery__sulla_constitutional_reaction, sullan_settlement_profiteers).
narrative_ontology:constraint_beneficiary(crisis_machinery__sulla_constitutional_reaction, landholding_optimates).
narrative_ontology:constraint_victim(crisis_machinery__sulla_constitutional_reaction, proscribed_and_heirs).
narrative_ontology:constraint_victim(crisis_machinery__sulla_constitutional_reaction, republican_institutions).
narrative_ontology:constraint_victim(crisis_machinery__sulla_constitutional_reaction, dispossessed_property_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PROSCRIBED (SNARE) — Named on the death lists, their property confiscated and sold at auction, their legal existence erased. No exit option exists within the framework of the dictatorship. Maximum coercion, maximum extraction of wealth, no meaningful coordination benefit. The proscription is pure suppression mechanism with no pretense of coordination.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEIRS OF THE DISPOSSESSED (SNARE) — Trapped across generations. Property confiscated under the dictatorship is locked into the hands of Sullan beneficiaries. Heirs cannot recover ancestral estates or social position without overturning the Sullan settlement itself. The extraction mechanism (settled property ownership, legal irreversibility) persists across the dictator's death. Trapped at biographical and generational horizons.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SULLAN BENEFICIARIES (TANGLED ROPE) — Landholding optimates and military officers who purchase proscribed estates at auction. Experience genuine coordination benefit: the dictatorship creates legal certainty, property rights enforcement, and a stable oligarchic settlement. Also experience extraction: must support the military apparatus and executive authority that maintains the settlement. Arbitrage exit available (can withdraw from political life, maintain land); beneficiary status is clear. Coordination + extraction in one system.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SENATE (SCAFFOLD) — Formally delegated authority to Sulla 'to write the constitution.' The Senate perceives the dictatorship as temporary authority to restore the state and then resign office (the Cincinnatus model). Low theater at the immediate horizon — the Senate sees explicit authorization and a defined task. But the actual implementation (indefinite tenure, proscription) violates the scaffold frame. If Sulla actually resigned and restored the Republic, this is scaffold; if he inverts the form, it becomes snare retroactively. The Senate's perspective is the aspirational frame that masks the actual constraint.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN INSTITUTIONS (PITON) — The dictatorship is formally supposed to be a temporary exception that restores the Republic. But the indefinite tenure, the proscription machinery, and the property transfers create a de facto permanent oligarchic settlement. The republican forms (Senate, consulates, assemblies) persist but are degraded — they ratify decisions made by the dictator. Theater ratio is high (0.55+): the procedural forms are maintained while real authority is concentrated. The constraint persists through inertia and the normalization of 'emergency' authority.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational vantage, the Sullan dictatorship demonstrates the structural inversion: emergency authority framed as temporary (the scaffold narrative) becomes the vehicle for permanent extraction (suppression of opponents, confiscation of property, oligarchic settlement). The form is inverted — the office meant to be laid down after restoring the state becomes the mechanism for rebuilding the state on behalf of the beneficiary class. This is not a mountain (not an inherent limit on democratic governance) but a clear structural snare: indefinite authority + published execution lists + property confiscation = pure extraction, with the scaffold frame as theater.
constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crisis_machinery__sulla_constitutional_reaction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crisis_machinery__sulla_constitutional_reaction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crisis_machinery__sulla_constitutional_reaction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(crisis_machinery__sulla_constitutional_reaction, TR),
    TR >= 0.70.

:- end_tests(crisis_machinery__sulla_constitutional_reaction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The dictatorship's primary mechanism is confiscation of property from proscribed individuals and redistribution to beneficiaries. Over the interval (0 to 10 years), extractiveness rises from 0.45 to 0.68 as the proscription scope expands and more property transfers occur. This is not maximal extraction (0.82+) because some beneficiaries genuinely receive coordination benefits (legal certainty, property rights enforcement, an oligarchic settlement) that go beyond pure extraction. But the extraction is substantial and structural. Suppression (0.82): Very high. The proscription lists are the primary suppression mechanism. The coercion is formalized, published, and escalating — early proscriptions target political enemies; later ones expand to include wealthy individuals whose property is desired. The suppression increases from 0.60 to 0.82 over the interval as the machinery becomes normalized and expanded. Theater ratio (0.55): Moderate. The dictatorship maintains republican procedural forms — the Senate delegates authority, the dictator formally 'writes the constitution,' property transfers are legal (although based on confiscation). But the actual mechanism (proscription as execution device, confiscation as transfer method) is not heavily theatrical — the death lists are public and direct, not hidden behind elaborate ritual. Theater increases slightly over time (0.40 to 0.55) as the rationalization of the settlement occurs and republican forms are invoked more frequently to justify ongoing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Senate's perspective (scaffold — temporary authority to write a constitution) and the actual mechanism (snare — indefinite authority to extract via proscription) is the core diagnostic feature of this constraint. The Senate perceives the Cincinnatus model — emergency authority with a sunset — but Sulla inverts the form. The beneficiary class perceives themselves as receiving a stable oligarchic settlement (tangled rope — coordination + some extraction), but the dispossessed and their heirs perceive pure extraction and legal erasure (snare). The republican institutional forms perceive themselves as degraded but functional (piton), while the analytical observer sees them as hollowed out and servile to the dictator's purposes. Each perspective is measuring a real aspect of the constraint, but the gap between the Senate's frame (this is temporary constitutional reform) and the structural reality (this is permanent oligarchic rule financed by confiscation) reveals that the constraint inverts the form it claims to exemplify.
 *
 * DIRECTIONALITY LOGIC:
 *   The proscribed and their heirs are trapped agents bearing full extraction — high d, maximum f(d). The Sullan beneficiaries are powerful arbitrage agents who benefit from the dictatorship — low d, negative or low χ for them. The Senate and republican institutions are constrained institutional actors caught between the formal authority they delegated and the actual power Sulla wields — moderate d. The analytical observer sees the full structural inversion at civilizational scope. The beneficiary class experiences genuine coordination benefit (property rights, legal settlement, political stability) which would normally qualify this as Tangled Rope. But the extraction mechanism (proscription + confiscation) is so substantial and the exclusion of alternatives so complete that the constraint classifies as Snare even from the beneficiary perspective — the settlement is achieved through suppression, not coordination. The constraint is not 'coordination plus extraction' but 'extraction masquerading as constitutional reform.'
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Sulla reading demonstrates how emergency authority can be inverted from a temporary exception into a permanent extraction mechanism. The classical dictatorship was legitimate (term-limited, with sunset) because it restored the state and then resigned. Sulla's dictatorship is delegitimized (indefinite, with proscription machinery) because it rebuilds the state for the benefit of a narrower class and never lays down the office. The reading coheres: it is not saying 'all dictatorships are snares' but 'indefinite dictatorships financed by proscription and confiscation are snares, while term-limited dictatorships with sunset are scaffolds.' The distinction turns on whether the emergency authority is used to restore the prior system or to construct a new permanent settlement at the expense of those excluded from the new order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincere_constitutional_intent,
    'Did Sulla genuinely intend to write a constitution and resign, or was constitutional reform the cover story for indefinite oligarchic rule from the start?',
    'Comparative analysis of Sulla''s prior statements about ending dictatorship vs actual behavior; examination of constitutional texts written vs actual property transfers and proscription scope; counterfactual: if Sulla had resigned after writing the constitution, would the settlement have survived without the dictator''s enforcement?',
    'If genuine: some beneficiaries genuinely received coordination benefit (stable legal framework), and the classification softens toward Tangled Rope. If cover story: the entire ''constitutional reform'' frame is theater masking pure extraction, confirming Snare across all beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_constitutional_intent, empirical, 'Whether Sulla''s constitutional intent was sincere or pretext').

omega_variable(
    property_settlement_durability,
    'Would the Sullan property settlement (confiscated estates redistributed to beneficiaries) have been durable without the dictator''s personal military force to back it, or did the constraint collapse when Sulla died?',
    'Historical analysis of property disputes post-Sulla; examination of whether beneficiary families retained confiscated estates without Sulla''s enforcement machinery; tracking of successful reversion claims by heirs; assessment of whether the settlement required continuous dictatorship to maintain',
    'If durable without the dictator: the settlement extracted permanently and the constraint remains Snare. If collapsed: the extraction was dependent on the dictator''s person and force, suggesting the constraint was Snare during dictatorship but reverted to Tangled Rope with reduced extraction post-Sulla.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_settlement_durability, empirical, 'Whether the property settlement survived the dictator''s death').

omega_variable(
    proscription_necessity_vs_choice,
    'Was the scale and method of proscription (published death lists, property confiscation) a necessary response to genuine political threat, or a chosen extraction mechanism that manufactured enemies to justify confiscation?',
    'Comparison of actual political opposition to the proscription scope; analysis of whose property was confiscated (genuine enemies vs wealthy rivals vs random victims); examination of whether alternative mechanisms (exile, temporary disenfranchisement, property fines) could have addressed legitimate security concerns without permanent extraction',
    'If necessary: suppression was a protective mechanism against real opposition, and the constraint is closer to Tangled Rope (coordination + extraction both essential). If chosen: proscription was a confiscation device and the constraint is pure Snare. If manufactured: proscription created enemies to justify confiscation (meta-extraction), confirming Snare with additional theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proscription_necessity_vs_choice, empirical, 'Whether proscription was necessary response or chosen extraction mechanism').

omega_variable(
    reading_foreclosure_query,
    'Does the Sulla reading foreclose the term-limited dictatorship reading, or can both coexist as competing constitutional models?',
    'Normative/conceptual analysis: the term-limited model (Cincinnatus) asserts that dictatorship with a defined term and sunset is legitimate emergency governance; the Sulla reading asserts that indefinite dictatorship inverts the form and becomes extraction. Can one framework hold both claims, or does accepting the Sulla reading logically require rejecting the term-limited model?',
    'If forecloses: the Sulla reading''s commitment to ''emergency authority used to rebuild the state being broken'' directly contradicts the term-limited model''s premise that dictatorships can be legitimate if constrained. This is a hard logical conflict. If coexists: different historical contexts permit different models — the term-limited model works in moments of genuine threat with leaders willing to resign; the Sulla model describes what happens when those constraints fail.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_query, conceptual, 'Whether Sulla reading forecloses or coexists with term-limited model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crisis_machinery__sulla_constitutional_reaction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sulla_theater_t0, crisis_machinery__sulla_constitutional_reaction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sulla_theater_t5, crisis_machinery__sulla_constitutional_reaction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sulla_theater_t10, crisis_machinery__sulla_constitutional_reaction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(sulla_extraction_t0, crisis_machinery__sulla_constitutional_reaction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sulla_extraction_t5, crisis_machinery__sulla_constitutional_reaction, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(sulla_extraction_t10, crisis_machinery__sulla_constitutional_reaction, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sulla_suppression_t0, crisis_machinery__sulla_constitutional_reaction, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sulla_suppression_t5, crisis_machinery__sulla_constitutional_reaction, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(sulla_suppression_t10, crisis_machinery__sulla_constitutional_reaction, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crisis_machinery__sulla_constitutional_reaction, enforcement_mechanism).
narrative_ontology:affects_constraint(crisis_machinery__sulla_constitutional_reaction, senatus_consultum_ultimum).
narrative_ontology:affects_constraint(crisis_machinery__sulla_constitutional_reaction, dictatorship_term_limited).

% DUAL FORMULATION NOTE:
% The Sulla reading is one constraint within the crisis_machinery kernel family. The term-limited dictatorship and senatus consultum ultimum readings are separate constraint stories with their own ε values and classification profiles. The Sulla reading (ε=0.68, Snare) demonstrates the failure mode of indefinite emergency authority. The term-limited reading (ε<0.25, Scaffold) demonstrates the success case with constrained authority. These are not the same constraint viewed differently — they have distinct ε values and represent opposing positions on whether emergency authority can be made safe. Link the stories to show the logical and structural relationships between the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
