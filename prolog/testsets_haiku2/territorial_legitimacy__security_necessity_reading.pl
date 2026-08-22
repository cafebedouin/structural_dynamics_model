% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity and Defensive Control
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested
 *   territorial legitimacy kernel. The security-necessity reading asserts
 *   that Israeli territorial control extending beyond 1967 borders is
 *   legitimate because it provides strategic buffer zones necessary to
 *   prevent existential military threats from adversaries positioned in those
 *   territories. Under this reading, Palestinian sovereignty is conditional
 *   on demilitarization (Palestinian territory must remain incapable of
 *   hosting credible offensive military forces), and settlements in occupied
 *   territory are legitimate as security presences. This is a SINGLE reading,
 *   not a neutral summary: it takes the Israeli security establishment's
 *   threat assessment as the referent for legitimacy and treats security
 *   necessity as a sufficient warrant for territorial control. The sibling
 *   readings (partition_reading and indigenous_continuity_reading) disagree
 *   on the kernel's interpretation and would produce different constraint
 *   classifications; they are separate constraint files, not variations of
 *   this one.
 *
 * KEY AGENTS:
 *   - Israeli security establishment: institutional beneficiary and agenda-setter; frames security necessity as existential; controls military enforcement
 *   - Israeli settler movement: organized beneficiary and identity-locked participant; fused to ideological expansionism the security doctrine justifies
 *   - Palestinian population (West Bank and Gaza): powerless victims; bear primary material cost of occupation and dispossession
 *   - Arab state claimants (Syria/Golan): powerful payer; constrained by Israeli military dominance and de-facto sovereignty facts
 *   - Palestinian leadership: excluded from agenda-setting; negotiate only within pre-set security-doctrine frames
 *   - International legal community: observer seat; mostly rejects security-necessity reading as warrant for occupation
 *   - United States strategic alliance: beneficiary; gains regional military positioning from Israeli control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.81).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Defensive Control").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '12975a57-dccf-4827-bc22-2f4b0c4964ab').
narrative_ontology:cs_kernel_codification('12975a57-dccf-4827-bc22-2f4b0c4964ab', distributed).
narrative_ontology:cs_authority_grounding('12975a57-dccf-4827-bc22-2f4b0c4964ab', extraction).
narrative_ontology:cs_reading_relation('12975a57-dccf-4827-bc22-2f4b0c4964ab', territorial_legitimacy__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('12975a57-dccf-4827-bc22-2f4b0c4964ab', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('12975a57-dccf-4827-bc22-2f4b0c4964ab', foundational, security_necessity_overrides_partition_borders).
narrative_ontology:cs_axiom_status(security_necessity_overrides_partition_borders, holdable).
narrative_ontology:cs_axiom_grounding('12975a57-dccf-4827-bc22-2f4b0c4964ab', security_necessity_overrides_partition_borders, instrumental).
narrative_ontology:cs_axiom('12975a57-dccf-4827-bc22-2f4b0c4964ab', foundational, strategic_depth_existentially_required).
narrative_ontology:cs_axiom_status(strategic_depth_existentially_required, holdable).
narrative_ontology:cs_axiom_grounding('12975a57-dccf-4827-bc22-2f4b0c4964ab', strategic_depth_existentially_required, empirically_contingent).
narrative_ontology:cs_reference_frame('12975a57-dccf-4827-bc22-2f4b0c4964ab', defensive_buffer_zone_necessity).
narrative_ontology:cs_drift_state('12975a57-dccf-4827-bc22-2f4b0c4964ab', contemporary_post_2020_treaties, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('12975a57-dccf-4827-bc22-2f4b0c4964ab', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_state_institutional_continuity).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, arab_states_golan_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settler_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, united_states_strategic_alliance).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, israeli_settler_movement).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, arab_state_claimants_golan).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the security doctrine that legitimate Israeli territorial control extends to strategic buffer zones beyond 1967 lines. Administers military occupation, settlement authorization, and settlement-protection operations. Claims the arrangement exists to prevent existential threats from adversaries using controlled territory. Fused to the state's foundational identity narrative: Israeli security establishment institutional legitimacy rests on the claim that its territorial model prevents another Holocaust.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).

% Receives legitimacy and state protection for settlements in occupied territory, framed as defensive security presence. Settlers pay no occupation tax and receive subsidized services, military protection, and preferential resource allocation (water, land). Identity-locked: settler communities have fused their group identity and ideological worldview to the territorial expansionism the security doctrine justifies. Exit means abandoning ideological commitment and community belonging simultaneously.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settler_movement, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_settler_movement, payer).

% Bears the primary material cost of the security arrangement: subject to military law, permit systems for movement and resource access, home demolition for settlement expansion and military zones, dispossession framed as security necessity. Trapped: exit would require abandoning ancestral territory, livelihood, and community, with no state-sponsored alternative resettlement and no international enforcement of right-of-return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_west_bank, payer,
    powerless, biographical, trapped, regional).

% Subject to blockade, territorial confinement, and episodic military operations justified as security necessity. Trapped: exit requires departure from historical territory with no legal right of return and no receiving state willing to absorb Palestinian refugees permanently.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population_gaza, payer,
    powerless, biographical, trapped, regional).

% Syria and other Arab states that claim the Golan Heights as occupied territory (Israeli security doctrine treats control as necessary buffer against hostile northern border). Constrained: military options are deterred by Israeli military superiority; diplomatic options are blocked by Israeli settlement facts and de-facto sovereignty claims; international law enforcement is absent.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, arab_state_claimants_golan, payer,
    powerful, generational, constrained, regional).

% Would challenge the security-necessity reading directly (asserting right to full territory, rejecting demilitarization conditions, denying that Israeli security requires occupied settlements). Excluded from agenda-setting: Palestinian representatives negotiate only within a framework the Israeli security establishment pre-sets, constraining the range of acceptable proposals to those compatible with maintained control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_leadership, excluded,
    moderate, biographical, constrained, regional).

% Assesses the security-necessity reading against international humanitarian law, the UN Charter, and the law of occupation. Most mainstream international legal authorities reject the claim that security necessity legitimates permanent settlement expansion; the Israeli security establishment rejects the authority of external legal review over its existential security judgments.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Gains regional strategic stability and an allied military presence from Israeli control of elevated terrain, buffer zones, and unified command structure. Receives no direct territorial transfer but benefits from predictable military coordination and forward positioning against competitors. Mobile: could shift alliance weight or support alternative arrangements, but current arrangement distributes geopolitical advantage favorably.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, united_states_strategic_alliance, beneficiary,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Israeli territorial and security administration under a coherent doctrine such that military decisions (settlement authorization, barrier placement, occupation law, military operations) follow from a single threat-assessment framework rather than ad-hoc conflict response.
% TRANSFER_FUNCTION: Moves Palestinian territorial rights and self-determination capacity to Israeli security administration; moves resource access (water, land, airspace) from Palestinian governance to Israeli control; moves international legitimacy from UN partition frames to Israeli security-necessity frames.
% ABSENT_VOICES: Palestinian political leadership is excluded from agenda-setting; they negotiate only within pre-set security-doctrine frames. Alternative Israeli security doctrines (those accepting 1967 borders as defensible without continued occupation) are marginalized within Israeli security establishment discourse. International legal authorities and human-rights monitors object continuously but have no enforcement mechanism or recognized standing in the constraint's operational logic.
% DISAPPEARANCE_RATIONALE: Israeli security establishment: removal would recreate 1967 vulnerability to invasion. Palestinian leadership and human-rights organizations: removal would restore Palestinian rights and reduce the existential threat that occupation itself creates. The disappearance verdict cannot be neutral — the constraint's structure ensures that its disappearance is precisely what the parties contest.
% FOUNDING_PROBLEM: 1948 Israeli independence left the state surrounded by larger Arab states with hostile intentions and superior numbers; 1967 war revealed vulnerability to multi-front invasion; the security doctrine justified territorial control and buffer zones as necessary to prevent existential threats.
% FOUNDING_PROBLEM_CORROBORATION: Israeli security establishment attests the threat is perpetual and requires maintained control. Palestinian leadership and international military analysts attest the original threat (multi-front conventional invasion) has been substantially eliminated by Israeli military dominance, peace treaties with Egypt and Jordan, and mutual deterrence. No corroboration of the security-necessity claim comes from outside the Israeli security establishment and its allied actors; Palestinian voices and international observers dispute the threat assessment.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, contested).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.78 because the constraint transfers Palestinian territorial rights, resource access, and political sovereignty to Israeli security administration, with the transfer justified by the Israeli security establishment's unilateral assessment of necessity. The transfer is not consensual and Palestinian consent is treated as irrelevant (security necessity overrides agreement requirements). Suppression is 0.81 because the arrangement's persistence depends on active enforcement (military occupation, settlement protection, barrier construction, permit systems), not on Palestinian acceptance; Palestinian resistance is continuous and suppression must be maintained to prevent the arrangement's collapse. Theater ratio is 0.42 (moderate-rising): the real security function exists (threat prevention, deterrence), but a growing share of occupation activity is settlement-protection and territorial consolidation rather than pure threat-prevention. The temporal series show extractiveness and suppression rising through the first half of the interval and plateauing by the end: this pattern fits expansion-and-consolidation (settlements expand, enforcement infrastructure hardens, then stabilizes at a new equilibrium). The measurement grid is shared across all three metrics: every metric is authored at every time point (0, 7, 14, 21, 28, 35, 42, 49, 56), preventing the temporal misalignment that would inject false type-transitions.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli security establishment and Palestinian leadership (and Palestinian population) should compute to different constraint types. From the Israeli security seat, the arrangement is tangled_rope: genuine security coordination (preventing credible military threats) coupled with asymmetric enforcement (Palestinians bear the movement-restriction and dispossession costs). From the Palestinian seat, the arrangement is snare: security-necessity framing is a cover story for territorial and resource extraction, with the Palestinian population as trapped victims. The engine computes this divergence from directionality: Israeli security apparatus has d ~ 0.2 (beneficiary position, mobile exit), Palestinian population has d ~ 0.95 (target position, trapped exit). The authored claim is tangled_rope (the reading's own classification), but the metrics describe substantially extractive, enforcement-dependent operation that could compute as snare from the victim seat. This gap is the measurement the framework exists to make transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli security establishment (agenda_setter, institutional power, identity_locked exit): low directionality (~0.15), beneficiary. The security apparatus sets the constraint and controls its enforcement; it benefits from the constraint's operation (consolidated territorial control, resource access, institutional legitimacy). Exit is identity-locked: the security establishment's institutional identity and legitimacy are fused to the security doctrine; questioning the doctrine means questioning the institution's raison d'etre. Beneficiary status is clear. Israeli settler movement (organized power, identity_locked exit): d ~ 0.25, beneficiary. Settlers receive state protection, subsidized services, and ideological legitimacy for occupying Palestinian territory; their exit is identity-locked (settler identity is group-defined and ideologically fused to territorial expansionism). Palestinian population (powerless, trapped exit): d ~ 0.95, victim. Palestinians bear the primary material cost (dispossession, movement restriction, resource scarcity, violence exposure); they have no exit (leaving means abandoning territory and community; receiving states will not accept permanent Palestinian refugee populations). Arab state claimants (powerful but constrained exit): d ~ 0.75, victim/target. They have territorial claims the constraint forecloses; they have military power but deterrence prevents its use (constrained rather than mobile exit); they are targets of the constraint even though their power is substantial. Palestinian leadership (moderate power, constrained exit): d ~ 0.85, excluded-payer. They would reject the security-necessity reading and advocate Palestinian sovereignty, but they negotiate only within frames pre-set by the Israeli security establishment; their exclusion from agenda-setting is structural. United States (powerful, mobile exit): d ~ 0.3, beneficiary. The US gains strategic positioning and allied military presence from the constraint; it could shift support but current arrangement distributes advantage favorably; exit is mobile (the US could realign its interests).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandate erosion. The founding problem (1948 vulnerability to invasion by larger Arab armies) has been substantially addressed by: (1) Israeli military dominance (conventional and nuclear); (2) peace treaties with Egypt (1979) and Jordan (1994), which eliminated the largest military threats; (3) mutual deterrence; (4) Arab state militaries weakened by internal conflicts. The original security necessity argument — that Israeli control of high ground and buffer zones is required to prevent invasion — is weaker in 2024 than in 1967. However, the security-necessity doctrine has evolved to claim new threats (terrorism, rocket fire, hostile state rearmament) that justify retention of the occupied territories. The constraint persists not because the founding problem is live in its original form, but because the security doctrine has become self-perpetuating: the arrangement itself generates resistance and counter-threats that the doctrine uses to justify continued control. This is classic mandate atrophy: the original warrant has weakened, but the arrangement's beneficiaries have reframed the warrant to make it self-justifying. Mandatrophy is contested (omega variable) — the Israeli security establishment claims threats are perpetual, while Palestinians and many international observers claim the founding problem is dead. The rising theater_ratio (0.25 → 0.42 over the interval) is consistent with increasing theatrical maintenance: security enforcement is increasingly performative (settlement-protection, barrier-maintenance, periodic operations) rather than threat-response.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_threshold,
    'What level of territorial control is actually necessary for Israeli security, and does current control exceed that threshold?',
    'Military capability assessment: modeling Israeli security under alternative territorial arrangements (1967 borders with demilitarized Palestinian zones, international guarantees, defensive fortifications); comparing threat elimination under each arrangement.',
    'If substantial control is security-necessary but current control exceeds necessity, the extraction component (the excess) is snare-like while the necessary component is rope-like. If all current control is necessary, tangled_rope classification holds. If no territorial control beyond 1967 borders is necessary, the entire reading collapses to snare (security framing is cover for extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_necessity_threshold, empirical, 'Whether current territorial control exceeds military necessity threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(terr_tr_t7, territorial_legitimacy__security_necessity_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(terr_tr_t14, territorial_legitimacy__security_necessity_reading, theater_ratio, 14, 0.31).
narrative_ontology:measurement(terr_tr_t21, territorial_legitimacy__security_necessity_reading, theater_ratio, 21, 0.35).
narrative_ontology:measurement(terr_tr_t28, territorial_legitimacy__security_necessity_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement(terr_tr_t35, territorial_legitimacy__security_necessity_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(terr_tr_t42, territorial_legitimacy__security_necessity_reading, theater_ratio, 42, 0.42).
narrative_ontology:measurement(terr_tr_t49, territorial_legitimacy__security_necessity_reading, theater_ratio, 49, 0.42).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy__security_necessity_reading, theater_ratio, 56, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(terr_be_t7, territorial_legitimacy__security_necessity_reading, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(terr_be_t14, territorial_legitimacy__security_necessity_reading, base_extractiveness, 14, 0.62).
narrative_ontology:measurement(terr_be_t21, territorial_legitimacy__security_necessity_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(terr_be_t28, territorial_legitimacy__security_necessity_reading, base_extractiveness, 28, 0.73).
narrative_ontology:measurement(terr_be_t35, territorial_legitimacy__security_necessity_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement(terr_be_t42, territorial_legitimacy__security_necessity_reading, base_extractiveness, 42, 0.77).
narrative_ontology:measurement(terr_be_t49, territorial_legitimacy__security_necessity_reading, base_extractiveness, 49, 0.78).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy__security_necessity_reading, base_extractiveness, 56, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(terr_su_t7, territorial_legitimacy__security_necessity_reading, suppression_requirement, 7, 0.71).
narrative_ontology:measurement(terr_su_t14, territorial_legitimacy__security_necessity_reading, suppression_requirement, 14, 0.74).
narrative_ontology:measurement(terr_su_t21, territorial_legitimacy__security_necessity_reading, suppression_requirement, 21, 0.77).
narrative_ontology:measurement(terr_su_t28, territorial_legitimacy__security_necessity_reading, suppression_requirement, 28, 0.79).
narrative_ontology:measurement(terr_su_t35, territorial_legitimacy__security_necessity_reading, suppression_requirement, 35, 0.8).
narrative_ontology:measurement(terr_su_t42, territorial_legitimacy__security_necessity_reading, suppression_requirement, 42, 0.81).
narrative_ontology:measurement(terr_su_t49, territorial_legitimacy__security_necessity_reading, suppression_requirement, 49, 0.81).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy__security_necessity_reading, suppression_requirement, 56, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.14).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel decomposes into three constraint stories, one per reading: security_necessity_reading (this file), partition_reading (international legal partition warrant), and indigenous_continuity_reading (Palestinian self-determination and anti-colonial continuity warrant). Each reading has its own ε, its own beneficiary/victim structure, and its own type classification. The three stories are linked via network.affects_constraints: the security-necessity reading influences both siblings by establishing security-necessity as a legitimate override of partition borders and indigenous rights claims. Sibling readings would compute different types from different seats, producing seat divergence on the same territorial phenomenon. The manifest ε for this reading (0.78) is the extraction value under the security-necessity reading's own lights; the sibling readings would author their own ε values reflecting their different assessments of legitimacy and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
