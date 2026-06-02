% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate: Dual Obligation to Indigenous Rights and Self-Determination
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the Balfour mandate kernel:
 *   the interpretation that mandate instruments impose equal or superior
 *   obligation to protect existing Arab civil/political rights and land
 *   tenure, with the 'national home' explicitly subordinated to
 *   self-determination norms and minority-protection principles. This is the
 *   dual-obligation reading. It is one of three structurally distinct
 *   interpretations of the same mandate text. The reading is historically
 *   grounded in League of Nations mandate doctrine (Article 22 of the
 *   Covenant, Permanent Mandates Commission jurisprudence) and Arab political
 *   claims throughout the mandate period. The constraint exhibits genuine
 *   Tangled Rope structure: the dual-obligation framework provides real
 *   coordination benefits (reconciles competing claims through quota and
 *   land-transfer mechanisms) while imposing asymmetric extraction
 *   (constraining Zionist expansion and British accommodationist impulses).
 *   The extractiveness trajectory shows accumulation from 1920-1940 as
 *   Zionist organizational power pressured quotas and land restrictions
 *   upward, forcing British enforcement costs to rise. Theater ratio shows
 *   increasing performative content: British administrators increasingly
 *   cited dual-obligation language while facilitating settlement expansion,
 *   hollowing the constraint's functional force. The false-summit perspective
 *   (analytical/civilizational) risks naturalizing this institutional choice
 *   as an immutable sovereignty conflict, masking the contingent power
 *   structures and interpretive contests that sustain or undermine
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Palestinian Arab communities and elites: Beneficiary (moderate/constrained) — gains formal legal status through dual-obligation framework; Arab majority grounds sovereignty and representative-government claims; land-transfer restrictions protect tenure. However, constrained by British discretion and subject to erosion by Zionist expansion despite quotas.
 *   - Zionist organizations and settlement movement: Victim (organized/constrained) — structurally blocked by land-transfer restrictions, immigration quotas, and Arab majority-status grounds for political authority. Organized power insufficient against dual-obligation enforcement; constrained expansion relative to unrestricted settlement scenarios.
 *   - British mandatory administrators: Bifurcated (institutional/constrained) — coordinate between dual-obligation mandate terms and Zionist pressure. Constrained by mandate obligations but benefit from discretion; increasingly exercise this discretion toward Zionist accommodation, raising theater ratio.
 *   - League of Nations Permanent Mandates Commission: Institutional interpreter (institutional/arbitrage) — vested with authority to adjudicate mandate obligations; the dual-obligation reading would require League enforcement capacity against British deviations.
 *   - Arab civil/political rights framework: Beneficiary (analytical) — abstract legal principle; benefits from dual-obligation reading; victim if reading is subordinated or rendered performative.
 *   - Analytical observer: Detached position (analytical/analytical) — risks naturalizing the institutional choice as immutable territorial sovereignty, masking the interpretive and power-political contingencies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.48).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.62).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.48).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate: Dual Obligation to Indigenous Rights and Self-Determination").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '60e990aa-d4bb-486d-a73e-342f95680050').
narrative_ontology:cs_kernel_codification('60e990aa-d4bb-486d-a73e-342f95680050', fixed_text).
narrative_ontology:cs_authority_grounding('60e990aa-d4bb-486d-a73e-342f95680050', extraction).
narrative_ontology:cs_interpretation_layer_present('60e990aa-d4bb-486d-a73e-342f95680050').
narrative_ontology:cs_reading_relation('60e990aa-d4bb-486d-a73e-342f95680050', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('60e990aa-d4bb-486d-a73e-342f95680050', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('60e990aa-d4bb-486d-a73e-342f95680050', foundational, arab_majority_grounds_sovereignty).
narrative_ontology:cs_axiom_status(arab_majority_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('60e990aa-d4bb-486d-a73e-342f95680050', arab_majority_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('60e990aa-d4bb-486d-a73e-342f95680050', foundational, land_tenure_protection_superior_to_national_home).
narrative_ontology:cs_axiom_status(land_tenure_protection_superior_to_national_home, holdable).
narrative_ontology:cs_axiom_grounding('60e990aa-d4bb-486d-a73e-342f95680050', land_tenure_protection_superior_to_national_home, deontological).
narrative_ontology:cs_reference_frame('60e990aa-d4bb-486d-a73e-342f95680050', mandate_dual_obligation_framework).
narrative_ontology:cs_drift_state('60e990aa-d4bb-486d-a73e-342f95680050', post_1929_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60e990aa-d4bb-486d-a73e-342f95680050', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_civil_political_rights_framework).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandate_administrators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB MAJORITY (SNARE) — Trapped by external demographic and institutional pressure. Despite majority status and indigenous tenure, constrained by immigration quotas and land-transfer restrictions that are framed as protective but experienced as suppressive (preventing their own land acquisition). Cannot exit the mandate system; demographic displacement proceeds via settlement and institutional subordination. Maximum extraction from perspective of demographic self-determination.
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARAB POLITICAL ELITES (TANGLED ROPE) — Constrained by mandatory administration but benefit from formal recognition of Arab civil/political rights, minority-protection principles, and residual sovereignty claims grounded in majority status. The dual-obligation reading provides them negotiating leverage over Zionist demands, but enforcement of these protections requires continuous British affirmation and faces pressure from Zionist institutional power. Mixed coordination (rights framework) and extraction (demographic control).
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST ORGANIZATIONS (SNARE) — Organized but structurally blocked. Land-transfer restrictions, immigration quotas indexed to Arab majority consent, and Arab civil-political-rights protections constrain territorial expansion and demographic parity claims. The dual-obligation reading forecloses the 'national home' as primary obligation, subordinating Zionist ambitions to Arab self-determination norms. High suppression; organized power insufficient to override the mandate's indigenous-rights framing. Experienced as coercive extraction by the constraining institution.
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEAGUE OF NATIONS MANDATE SYSTEM (ROPE) — From the perspective of the mandate's formal legal framework, the dual-obligation reading IS the coordination mechanism: reconciling national-home aspiration with indigenous-rights protection through quotas, land-transfer restrictions, and Arab majority status as ground for representative government. The mandate system sees this as pure coordination—solving the problem of colonial transition while protecting existing rights. Low extraction from the mandate's self-view; the constraint enables legitimate institutional function.
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BRITISH MANDATORY ADMINISTRATOR (TANGLED ROPE) — Faces genuine coordination challenge: implement dual obligation to Arab rights AND satisfy Zionist demands. The dual-obligation reading constrains Zionist accommodation, requiring the British to enforce land-transfer restrictions and immigration quotas against Zionist pressure. Extraction runs both ways: British sovereignty is constrained by both mandate terms AND by institutional pressure from Zionist organizations (not bound by the dual-obligation reading). Theater rises as British administrators perform enforcement of protections while covertly facilitating Zionist expansion.
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: IMPERIAL COLONIAL ADMINISTRATION (PITON) — From the perspective of imperial power structures, the mandate system is a degraded compromise between formal self-determination principles (the kernel: League of Nations authority) and actual imperial control. The dual-obligation reading is performative: it invokes indigenous rights and self-determination norms while the actual institutional force is colonial administration and settlement facilitation. Theater_ratio reflects the gap between mandate rhetoric and imperial practice—the constraint persists through institutional inertia (League authority, legal forms) despite functional hollowing (actual power resides in imperial preference and settler expansion).
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, the tension between national home and indigenous rights may appear as an immutable property of sovereign state formation: two incompatible claims to the same territory cannot coexist; one must supersede the other. This perspective naturalizes the constraint as inherent to state-building. However, the structural data contradicts the mountain classification—the mandate system itself instantiates a legal framework (the dual-obligation reading) that rejects both immutability and natural resolution. The false-summit detection will flag this as naturalization of what is a contingent institutional choice.
constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(balfour_mandate_instruments__dual_obligation_indigenous_rights, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, TR),
    TR >= 0.70.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting the constraint's genuine coordination function (reconciling competing claims) alongside asymmetric extraction (constraining expansion). The baseline (1920) is lower (0.25) because the mandate framework is freshly imposed and both quotas and land-transfer restrictions are formally established. By 1940, extractiveness has risen to 0.48 as Zionist organizational pressure erodes quota enforcement and British administrators increasingly covertly facilitate settlement. The constraint provides coordination benefits (Arab rights framework, quota mechanisms, land-transfer procedures) that prevent pure extraction, keeping extractiveness below the 0.66 snare threshold. Suppression (0.62): Moderate-high. The constraint suppresses Zionist expansion through legal mechanisms (land-transfer restrictions, immigration quotas) and suppresses British discretionary accommodation through mandate terms. However, suppression is not total—Zionist organizations find pathways around restrictions (purchase through intermediaries, immigration via loopholes), and British enforcement degrades over time. Theater ratio (0.58): Moderate, reflecting increasing performative content. Early mandate (1920) exhibits lower theater (0.42) because the dual-obligation framework is relatively new and formally enforced. By 1940, theater rises to 0.58 as British administrators increasingly cite dual-obligation language while facilitating settlement expansion, creating gap between formal obligation and actual practice. The theater rise signals degradation toward Piton unless League enforcement capacity is activated.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading (dual-obligation) and its siblings is substantial. The jewish_national_home_primacy reading would place Zionist organizations as beneficiary (lower d, Rope from their perspective) and Arab communities as victims (higher d, Snare from their perspective)—a complete inversion. The mandatory_interpretive_discretion reading would place British authority as primary beneficiary (low d, Rope from their perspective, emphasizing coordination between competing claims) and both Arab and Zionist organizations as victims of discretion (moderate d, Tangled Rope from both). This constraint (dual_obligation) prioritizes Arab structural position as grounding the constraint's legitimacy, subordinating Zionist claims. The perspectival gap is NOT a disagreement about facts (everyone can observe the quotas and land-transfer restrictions) but about which reading's interpretation is binding—which authority structure (League, Arab majority, Zionist aspiration, British imperial discretion) takes priority in adjudicating the kernel's meaning. The false-summit perspective (analytical/civilizational) risks collapsing the reading contest by naturalizing the outcome as inevitable sovereignty conflict, obscuring the institutional and interpretive choices that sustain enforcement or enable degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   The dual-obligation reading is a kernel reading, not an observer-position reading. Its directionality (d) is established by declaring the structural beneficiaries (Arab communities, Arab rights framework) and victims (Zionist organizations, British administrative discretion). Arab communities are structurally mobile within the constraint (they are not trapped—they have political organization, land ownership, and population majority) but are constitutionally bounded by the constraint's terms (they cannot exceed the constraint's provisions without invalidating the framework that grounds their claims). Zionist organizations are structurally constrained (organized power, but externally bounded by quotas and restrictions) and constitute victims in the dual-obligation reading because the constraint blocks their expansion. British administrators are bifurcated—they benefit from discretionary authority (arbitrage exit option in imperial context) but are constrained by mandate obligations (constrained exit in League context). The perspectival gap arises from these differentiated directionalities: Arab elites see genuine coordination benefit plus suppression (Tangled Rope), Zionist organizations see pure constraint (Snare at organized level), League authority sees coordination mechanism (Rope), British administration experiences mixed coordination and constraint (Tangled Rope from different angle), and the imperial administration experiences degradation of mandate principle (Piton). No directionality override needed—the structural declarations drive d values appropriately through the canonical chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy risk—extractiveness (0.48) is below the 0.70 threshold. However, the measurement trajectory shows incipient drift toward higher extractiveness (0.25 → 0.48 over 20 years) driven by rising theater ratio (0.42 → 0.58). The constraint's stability depends on sustained enforcement of the dual-obligation reading's institutional implications. If League authority weakens or British administration increasingly invokes mandatory_interpretive_discretion (allowing imperial preference to override dual obligation), extractiveness could rise toward 0.60-0.70 range, triggering mandatrophy analysis. The present classification (Tangled Rope) is stable under conditions of genuine quota enforcement and land-transfer restriction enforcement. Degradation would follow from erosion of these enforcement mechanisms without corresponding change in the beneficiary/victim structure—a signature of transition toward Piton (performative constraint) or Snare (pure extraction as Zionist expansion overcomes Arab majority constraints).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_obligation_enforceability,
    'Are land-transfer restrictions and immigration quotas structurally enforceable constraints on the mandatory power, or performative gestures that the mandatory authority can override when imperial interest demands?',
    'Historical analysis of British enforcement actions: frequency of blocking Jewish land purchases, immigration quota enforcement, penalties for violations. Comparison with archival evidence of private communications regarding Zionist accommodation.',
    'If enforceable: constraint is genuinely Tangled Rope (coordination + extraction with real costs to violators). If performative: constraint degrades to Piton (the dual-obligation reading is theater maintained by League authority form while actual power flows toward Zionist expansion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_obligation_enforceability, empirical, 'Whether dual-obligation enforcement mechanisms are structurally binding or performative').

omega_variable(
    arab_majority_demographic_trajectory,
    'Does the dual-obligation reading (grounding Arab representative-government claims in demographic majority) rest on a stable demographic majority, or on a baseline that Zionist immigration is designed to erode?',
    'Demographic projection models comparing Arab and Jewish population growth under different immigration scenarios. Analysis of whether immigration quotas were indexed to maintain Arab majority or merely slow Zionist growth.',
    'If Arab majority is protected by quota enforcement: dual-obligation reading is structurally coherent (Tangled Rope with stable beneficiary). If majority is eroding toward parity despite quotas: dual-obligation reading depends on enforcing quotas at levels Zionist pressure will eventually overcome (unstable Tangled Rope, presaging shift toward Snare or Mountain of demographic inevitability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arab_majority_demographic_trajectory, empirical, 'Stability of Arab demographic majority under dual-obligation quota regime').

omega_variable(
    kernel_reading_contest,
    'Which reading of the Balfour mandate kernel—dual_obligation_indigenous_rights, jewish_national_home_primacy, or mandatory_interpretive_discretion—grounds the League of Nations'' actual interpretive authority and enforcement capacity?',
    'Textual analysis of League mandate documents, Permanent Mandates Commission decisions, Assembly resolutions. Analysis of which reading received institutional endorsement vs. which was invoked selectively. Assessment of whether the contest is resolved by textual interpretation or by divergent institutional commitments (League authority vs. British mandatory power vs. Zionist organizational power).',
    'If dual-obligation reading is League''s binding interpretation: this constraint is structurally validated (genuine Tangled Rope). If jewish_national_home_primacy prevails in League authority: this reading becomes subordinated (Tangled Rope degrades toward Piton as the dual-obligation framework becomes ornamental). If mandatory_interpretive_discretion is the operative reading: all three readings coexist_with each other, and the constraint''s type depends on which reading the British administrator invokes in each decision (meta-level instability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the Balfour mandate kernel is institutionally endorsed as binding').

omega_variable(
    mandate_vs_imperial_power_hierarchy,
    'When mandate dual-obligation obligations conflict with British imperial interests or Zionist organizational pressure, which authority structure prevails: League mandate authority, British imperial sovereignty, or Zionist institutional power?',
    'Case analysis of specific conflicts: instances where mandate obligations required action that British authorities resisted or ignored. Assessment of League enforcement capacity and whether League sanctions were ever imposed or threatened.',
    'If mandate authority prevails: constraint is genuine Tangled Rope with real suppression of Zionist expansion. If imperial sovereignty prevails: the dual-obligation reading is constrained (Rope at best, Piton likely) because British discretion overrides mandate terms. If Zionist power bypasses both: constraint collapses toward Mountain of demographic inevitability or Snare of Zionist expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_vs_imperial_power_hierarchy, empirical, 'Hierarchical authority resolution in conflicts between mandate, empire, and organized Zionist power').

omega_variable(
    false_summit_natural_sovereignty,
    'Is the mandate constraint a contingent institutional choice (a Tangled Rope that could be enforced differently), or does it rest on an implicit natural law that territory cannot satisfy incompatible sovereignties simultaneously (Mountain)?',
    'Examination of whether the mandate framework itself presupposes resolution by one party superseding the other (naturalizing Mountain logic) or whether it operationalizes a genuine dual-obligation compromise (Tangled Rope). Analysis of League documents asserting whether the mandate is temporary (presaging inevitable resolution toward Mountain) or permanent (instantiating Tangled Rope as stable form).',
    'If natural law: the constraint appears immutable from Mountain perspective, but the false-summit detector will flag beneficiary declarations (Zionist organizations as victims) as evidence of naturalization. If contingent institutional choice: the dual-obligation reading is genuinely Tangled Rope, and collapse toward Mountain signals breakdown of institutional enforcement rather than discovery of hidden law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_sovereignty, conceptual, 'Whether dual-obligation mandate is contingent institutional choice or natural law of sovereign territory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bmdi_theater_1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bmdi_theater_1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 10, 0.54).
narrative_ontology:measurement(bmdi_theater_1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(bmdi_extract_1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bmdi_extract_1930, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(bmdi_extract_1940, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_majority_status_and_representation).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_settlement_expansion_under_mandate).

% DUAL FORMULATION NOTE:
% The Balfour mandate kernel decomposes into three structurally distinct readings with different epsilon values and beneficiary/victim structures. This story (dual_obligation_indigenous_rights) is one reading. The siblings (jewish_national_home_primacy, mandatory_interpretive_discretion) are separate constraint stories with different ε values, classifications, and measurement trajectories. The sibling readings coexist as competing institutional interpretations of the same mandate text. Network edges link them as members of the balfour_mandate_instruments kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
