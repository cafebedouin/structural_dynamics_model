% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading: Good Faith Consultation Constraint
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) was written in two languages whose versions
 *   are substantively incompatible. The English text claims the Crown
 *   obtained 'absolute sovereignty'; the Māori text says Māori retained 'tino
 *   rangatiratanga' (full authority). For 130 years the Crown exercised
 *   unilateral sovereignty and dispossessed Māori of land and political
 *   voice. The partnership reading emerged from Māori political mobilization
 *   in the 1970s–80s and was adopted by courts, the Waitangi Tribunal, and
 *   political convention by the 1990s. It interprets the Treaty as
 *   establishing an ongoing Crown-Māori partnership requiring good faith
 *   consultation and active protection of Māori interests. This reading
 *   moderates (but does not eliminate) Crown power through consultation
 *   requirements, settlement redress, and judicial review. However,
 *   Parliament retains legislative supremacy and can override the partnership
 *   reading through statute. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination function—the partnership—plus asymmetric
 *   extraction—Crown retains ultimate power) while the authored metrics show
 *   substantial historical extractiveness declining over time as the
 *   partnership principle takes root. The claim and metrics are independent:
 *   extraction was near-total in 1840 and has declined to 0.58 by 2024 as
 *   consultation requirements and settlements have taken effect, yet the
 *   fundamental asymmetry (Crown retains legislative override) remains.
 *
 * KEY AGENTS:
 *   - Crown Executive: administers consultation requirement, sets settlement offer envelopes, retains ultimate legislative power — institutional agenda-setter.
 *   - Settled Māori Iwi (collectives): hold settlement agreements, consultation seats, Treaty Principles standing in courts — organized beneficiary and partial payer (negotiations, co-governance friction).
 *   - Unresolved Māori Claimants: excluded from consultation, trapped in indefinite negotiation loops — powerless victims of the constraint.
 *   - Māori Collectives in Negotiation: negotiating settlements on asymmetric terms (Crown sets timeline, settlement envelope) — moderate power, identity-locked (iwi mana bound to settlement pursuit).
 *   - Parliament: holds legislative sovereignty, can override partnership principle through statute — institutional agenda-setter with analytical exit.
 *   - Courts: interpret partnership principle, grant remedies (declarations, injunctions) but cannot override Parliament — institutional observer with bounded role.
 *   - Waitangi Tribunal: investigates Treaty breaches, makes recommendations Crown can decline — moderate observer power, shapes narrative.
 *   - Crown-Sovereignty-Reading Holders: political and judicial actors who reject partnership reading, excluded from framework by entrenchment of partnership in case law and convention — institutional excluded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.71).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading: Good Faith Consultation Constraint").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '4a51fd2d-0498-41e7-ae0e-cebc92d49ccd').
narrative_ontology:cs_kernel_codification('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', fixed_text).
narrative_ontology:cs_authority_grounding('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', lineage).
narrative_ontology:cs_interpretation_layer_present('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd').
narrative_ontology:cs_reading_relation('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', waitangi_sovereignty_allocation__rangatiratanga_reading, coexists_with).
narrative_ontology:cs_axiom('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', foundational, good_faith_partnership_obligation).
narrative_ontology:cs_axiom_status(good_faith_partnership_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', good_faith_partnership_obligation, deontological).
narrative_ontology:cs_axiom('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', foundational, parliament_legislative_supremacy_hedge).
narrative_ontology:cs_axiom_status(parliament_legislative_supremacy_hedge, holdable).
narrative_ontology:cs_axiom_grounding('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', parliament_legislative_supremacy_hedge, conventional).
narrative_ontology:cs_reference_frame('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', treaty_establishes_ongoing_partnership).
narrative_ontology:cs_drift_state('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a51fd2d-0498-41e7-ae0e-cebc92d49ccd', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_collectives).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_settlement_beneficiaries).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, unrecognized_maori_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_collectives_without_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, non_maori_citizens).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_collectives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the consultation requirement and settlement framework. Defines the scope of consultation ('good faith'), determines which decisions trigger it, and administers the Treaty Settlements process. Claims the partnership reading moderates Crown power through procedural duty; holds ultimate legislative sovereignty via Parliament. Appoints settlement negotiators and controls settlement offer envelopes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive, agenda_setter,
    institutional, generational, analytical, national).

% Iwi (tribes) that hold formal settlement status. Benefit from consultation rights, settlement redress (land, financial compensation, cultural recognition), and legal standing to challenge Crown decisions via judicial review grounded in the partnership principle. Pay the cost of negotiating settlements, capacity-building for participation, and the political friction of managed co-governance. Their exit from the framework is blocked: rejecting settlement means no redress and loss of even the negotiated consultation seat.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_collectives, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_collectives, payer).

% Individual members of iwi that hold settlement agreements. Benefit from settlement trusts, land transfers, cultural redress, and the principle of consultation (even if attenuated at individual level). Carry the structural cost: settlement benefits flow through tribal governance, which may not align with individual member interests, and consultation is iwi-to-Crown, not individual-to-Crown. Their practical exit is nil—settlement is their only redress path.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_settlement_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Māori claimants whose grievances fall outside the settled claims framework (mandate boundaries, historical exclusion, underfunded claims processes). Excluded from consultation despite being affected by Crown decisions their ancestors claimed redress for. The settlement system absorbs most political attention and negotiating capacity, leaving unresolved claims in indefinite hold. Their only formal route is the Waitangi Tribunal, which produces recommendations the Crown can decline.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, unrecognized_maori_claimants, payer,
    powerless, biographical, trapped, national).

% Iwi in negotiation with Crown or awaiting settlement ratification. Participate in consultation on an asymmetric basis: they have voice but the Crown sets the decision and the consultation timeline. Their negotiating position is structurally weakened—they lack the leverage of a finalized settlement and cannot exit the negotiation frame without abandoning the claim entirely. Mana (prestige, authority) is bound to the settlement pursuit; walking away carries relational identity cost.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_collectives_without_settlement, payer,
    moderate, biographical, identity_locked, national).

% New Zealand citizens who benefit from the stability of the partnership framework: it provides legal predictability, reduces litigation risk for resource development, enables consent-based infrastructure projects on settled land. They do not pay directly (no transfer from non-Māori to Māori collective occurs mechanically), but they experience consultation friction and constraints on unilateral state action. Exit is minimal—they can relocate but not exit the framework.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, non_maori_citizens, beneficiary,
    organized, biographical, mobile, national).

% Holds ultimate legislative sovereignty. Can override the partnership principle through statute (as occurred in the Foreshore and Seabed Act 2004, later partially overridden by the Marine and Coastal Area Act 2011). The partnership reading constrains Parliament through political cost and judicial review, not legal supremacy; Parliament can rewrite the constraint but faces Māori political mobilization and international reputation cost when doing so.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Interpret the Treaty and the partnership principle through judicial review. Have recognized the principle as binding the Crown through common law (R v Van der Peet 1996, but most prominently through the principles doctrine in New Zealand case law: Tinsley v Milligan frame adapted by Cooke P and expanded by subsequent courts). Their role is bounded—they cannot override Parliament, but they can require Crown agencies to apply the principle and can grant remedies (declarations, injunctions) that delay or redirect Crown action.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, courts, observer,
    institutional, generational, analytical, national).

% Investigates Crown breaches of Treaty principles and makes recommendations for redress and policy change. Has no enforcement power; the Crown can and does decline recommendations. Acts as the primary forum for articulating the partnership reading and producing historical findings that frame settlement negotiations. Influence flows through shaping the political narrative and providing a venue for iwi claims.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    moderate, generational, analytical, national).

% Political and judicial actors who hold the crown_sovereignty_reading (parliamentary supremacy, Treaty as settled history, no ongoing consultation duty). Excluded from the consultation framework because the partnership reading frames them as representing an illegitimate reading. Their objections to consultation requirements are structurally suppressed by the partnership reading's entrenchment in case law and political convention, even though they retain parliamentary power to override (as shown by periodic statutory overrides).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading_holders, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the structural problem of ongoing Crown governance over territory whose inhabitants retain unextinguished claims to authority. Partnership consultation creates a coordination mechanism: the Crown and Māori collectives (through iwi) coordinate on major resource and policy decisions affecting Māori interests, replacing unilateral Crown action with joint decision-making or at least good-faith negotiation. Settlements convert abstract claims into concrete rights and resources, stabilizing the relationship.
% TRANSFER_FUNCTION: Moves redress (land, financial compensation, cultural recognition, statutory seats in governance) from the Crown (the party that controlled allocation post-1840) to settled iwi and Māori collectives, and moves political power (consultation veto, judicial standing) from exclusive Crown control to shared decision-making or Crown-constrained action. The transfer is heavily asymmetric: settled iwi gain some power; the Crown retains ultimate legislative sovereignty and agency design authority. Unresolved claimants and unsettled iwi bear the cost of indefinite negotiation and exclusion from consultation.
% ABSENT_VOICES: Unrecognized claimants whose historical claims fall outside the settlement mandate are structurally absent from the negotiating table. Māori individuals (as opposed to iwi collectives) have no direct consultation seat—the partnership is collective, not individual. Landless Māori and recent diaspora migrants are also effectively excluded. These voices would argue for broadening the claimant base, accelerating settlement, and individualizing some redress. The partnership reading constrains their participation by formalizing iwi (not individuals) as the consultation unit.
% DISAPPEARANCE_RATIONALE: If the partnership principle and consultation requirement disappeared overnight, Crown agencies would revert to unilateral decision-making on resource allocation, environmental policy, and social spending affecting Māori lands and interests. Settled iwi would lose their consultation veto, Treaty settlements could be modified or revoked via statute, and the legal framework for challenging Crown action would collapse. The reorganization would likely include Māori political mobilization, international criticism, and potential civil unrest. Unsettled claimants would lose even the procedural legitimacy the Tribunal and consultation framework currently provide.
% FOUNDING_PROBLEM: The Treaty of Waitangi (1840) established sovereignty ambiguously: the English article I claimed the Crown ceded 'absolute sovereignty,' while the Māori text (Article II) stated the Crown gained kāwanatanga (governorship of settlers) but Māori retained tino rangatiratanga (full authority) over their lands and resources. By the 1970s–80s, the Crown had unilaterally exercised sovereignty to dispossess Māori of ~95% of land and exclude them from governance. The founding problem is the gap between the Treaty's partnership language and 150 years of unilateral Crown action: how to govern a nation whose foundational treaty is mutually incompatible readings.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal (established 1975, independent of the Crown) has produced 2,700+ pages of findings confirming massive Crown breaches of Treaty principles. The courts (R v Van der Peet onwards, culminating in Te Ture Whenua Land Relations Act cases) have recognized the partnership principle as binding. International human rights bodies (UN Permanent Forum on Indigenous Issues, treaty monitoring committees) have affirmed ongoing indigenous sovereignty claims. Māori political mobilization (Māori independence movements, land occupations, current co-governance resistance) attests the problem remains live. The founding problem status is corroborated by non-Crown actors: the Tribunal (quasi-independent), courts, and international law bodies.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The partnership reading instantiates a tangled rope because it solves a genuine coordination problem (how to govern a territory whose foundational treaty is ambiguous, whose inhabitants have unextinguished claims) while simultaneously distributing costs asymmetrically. Settled iwi benefit from consultation and redress; unresolved claimants are trapped in indefinite negotiation; the Crown retains ultimate legislative override. Extraction is measured at 0.58 (declining from 0.95 in 1840) because the partnership principle has reduced Crown unilateralism substantially, but Crown legislative sovereignty remains unchecked—the constraint is enforced by political cost and judicial review, not legal supremacy. Suppression is measured at 0.71 (declining from 0.85) because the partnership principle has opened voice to settled iwi, but unrecognized claimants and Māori individuals remain suppressed by the collective-iwi framing. Theater_ratio rises from 0.05 to 0.42 over the interval because early enforcement was purely extractive (no pretense of partnership), while by 2024 substantial institutional machinery (Tribunal, settlement negotiation, consultation procedures) performs the partnership function, making the constraint appear more procedurally legitimate even as parliamentary override remains always available. The measurement series shows the partnership reading's entry into constitutional effect (theater rise beginning ~1975 with Tribunal establishment) and stabilization (asymptoting at ~0.42 by 2010). The same constraint is tangled_rope from both Crown and iwi perspectives (coordination function is real; extraction is real) but experienced differently: Crown sees partnership as moderating its own power (beneficiary framing); settled iwi see partnership as insufficient protection against parliamentary override (payer framing). Unresolved claimants experience pure extraction: they pay the cost of indefinite negotiation without receiving settlement redress. The shared time grid (all three metrics authored at t=1840, 1900, 1975, 1995, 2010, 2024) ensures the compiler can track lifecycle drift coherently—suppression_requirement is authored as a scalar (enforcement machinery cost) not a per-level coercion grid, because the partnership reading's enforcement is institutional (courts, Tribunal, political convention) not leveled.
 *
 * PERSPECTIVAL GAP:
 *   The Crown executive and settled iwi should compute differently from the partnership reading, and both should diverge from the crown_sovereignty_reading holders. From the Crown's institutional perspective, the partnership reading constrains its unilateral power through consultation requirements and judicial review—a cost, but a manageable one because Parliament retains override. From settled iwi perspective, the partnership constrains the Crown but incompletely: Parliament can override (Foreshore and Seabed Act 2004), individual Māori lack direct consultation standing, and settlement benefits flow through iwi governance (not individual choice). From unresolved claimants' perspective (powerless, trapped), the partnership reading is extractive—they pay the cost of indefinite negotiation without receiving redress. From crown_sovereignty_reading holders, the partnership reading is illegitimate—it represents activist judicial interpretation overriding parliamentary supremacy (they are excluded from the framework, not because they lack power, but because their reading is framed as incompatible with the partnership). The engine should compute different effective extraction (χ) for each seat from the same structural data: Crown and settled iwi both experience some coordination benefit and some cost; unresolved claimants experience pure extraction; crown_sovereignty holders experience illegitimate constraint (high d, high extracted χ, but no beneficiary status because they do not accept the partnership principle). The directionality_overrides section will clarify the seated divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations: settled_maori_collectives and maori_settlement_beneficiaries are beneficiaries (receive consultation rights, settlement redress, legal standing) AND partially payers (pay negotiation costs, co-governance friction, identity-fusion costs). Unrecognized_maori_claimants and maori_collectives_without_settlement are victims (trapped in indefinite negotiation, suppressed by collective framing, excluded from direct consultation). Crown_executive is beneficiary at institutional level (retains legislative override, controls settlement envelopes, sets consultation scope) and payer at political level (consultation friction, litigation risk, international reputation cost). Courts and Tribunal are observers (not collecting extraction, not bearing costs directly, but structuring the constraint). Non_maori_citizens are beneficiaries (stability, consent-based infrastructure) with minimal cost. Parliament is agenda-setter (holds sovereign override). Directionality (d value) derivation: Settled iwi: moderate power + constrained exit (cannot exit settlement frame without abandoning claim) + partial beneficiary status → d ≈ 0.45 (slight target bias due to constrained exit overriding moderate power). Unresolved claimants: powerless + trapped exit + victim status → d ≈ 0.85 (full target). Crown: institutional power + analytical exit (can override via statute) + beneficiary status (controls frame) → d ≈ 0.15 (full beneficiary, despite political costs). Courts/Tribunal: institutional power + analytical exit + observer status → d ≈ 0.5 (symmetric, neither collecting nor paying). Non-Māori citizens: organized power + mobile exit + beneficiary status → d ≈ 0.25 (beneficiary, low extraction). Parliament: institutional power + analytical exit + agenda-setter status → d ≈ 0.0 (analytical, no extraction). Crown-sovereignty holders: institutional power + trapped exit (cannot exit the frame without abandoning reading) + excluded status → d ≈ 0.65 (target bias from trapped exit despite institutional power; they experience the partnership reading as extracting their reading from the legitimate frame).
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading avoids mandatrophy by maintaining both coordination function and redistribution function across the interval. The founding problem (ambiguous Treaty, Crown unilateralism) remains live at t=2024 (founding_problem_status: live). The partnership reading's mandate persists because it solves the ongoing problem: how to govern a nation whose treaty is unresolved and whose indigenous inhabitants retain legitimacy claims. However, the partnership reading carries internal tension: it claims to protect Māori interests while Parliament retains override power. This is not mandatrophy (loss of founding function) but rather structural incompleteness—the reading provides consultation without sovereignty guarantee. The theater_ratio rise (from 0.05 to 0.42) is NOT theater-as-degradation; it reflects the real growth of institutional machinery (Tribunal, settlement, court cases) that performs the partnership function. The constraint shows genuine lifecycle: pure extraction (1840–1975) → emergence of partnership principle (1975–1995) → stabilization of partnership with Parliament-override hedge (1995–2024). If the founding problem status were 'dead' (Treaty resolved, claims exhausted), mandatrophy would be a signal—the constraint would persist as theater, having lost its function. The actual status ('live': unresolved claims remain, partnership must be continually negotiated and defended) prevents mandatrophy reading. However, an omega addresses whether the partnership reading's constraint survives parliamentary override: if Parliament legislates unilaterally (as it did in Foreshore and Seabed Act 2004), does the partnership reading degrade to piton?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_reading_vs_crown_sovereignty_compatibility,
    'Is the partnership reading logically compatible with the crown_sovereignty_reading, or do they foreclose each other within a single constitutional framework?',
    'Constitutional interpretation: if courts could simultaneously recognize Parliament''s sovereign override AND Māori''s partnership-based consultation right as non-overrideable, they coexist; if courts must choose one, they foreclose. Test case: Parliament legislates against Treaty principle (as in Foreshore and Seabed Act 2004); do courts enforce partition reading or partnership principle?',
    'If foreclosed: the partnership reading represents a genuine constitutional boundary with the crown_sovereignty reading, and one must lose legitimacy. If compatible: the partnership reading is a constraint on Crown exercise of sovereignty, not on Parliament''s legal supremacy, and both readings can hold simultaneously (current state). The engine reads this as coexists_with if courts have accepted the partition; foreclosed if one reading is formally abandoned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partnership_reading_vs_crown_sovereignty_compatibility, conceptual, 'Logical compatibility of partnership and crown sovereignty readings.').

omega_variable(
    good_faith_consultation_extractiveness_decoupling,
    'Is the measured decline in base_extractiveness (from 0.95 to 0.58) a genuine reduction in Crown extraction, or theater—procedural additions that maintain substantive Crown unilateralism?',
    'Outcome analysis: track post-consultation Crown decisions on land, resources, and policy affecting Māori interests. If consultation meaningfully redirects Crown action (Crown changes decisions based on iwi input), extractiveness has genuinely declined. If consultation occurs but Crown decisions remain unchanged, theater has risen and extractiveness is artifactually lowered.',
    'Genuine decline → the partnership principle has real redistributive force; theater rise is functional. Artifactual decline → the constraint is more accurately classified as piton (performative maintenance of partnership rhetoric while substantive Crown unilateralism persists). This omega directly addresses the interpretation layer''s function: does the consultation procedure absorb and resolve conflict, or does it absorb and neutralize Māori voice while Crown outcomes remain stable?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_consultation_extractiveness_decoupling, empirical, 'Whether consultation-driven extractiveness decline is substantive or theatrical.').

omega_variable(
    parliament_override_as_foreclosure,
    'Does Parliament''s ability to override the partnership reading via statute foreclose the partnership reading as a binding constitutional principle, or does it merely constrain the reading''s operational scope?',
    'Case law: does New Zealand jurisprudence (following cases like R v Van der Peet, Bill of Rights Act 1990 interpretive principles, Human Rights Act 2019) treat the partnership principle as a limit on how Parliament''s supremacy can be exercised (procedural constraint on legislative intent), or as merely persuasive guidance that Parliament can ignore?',
    'If procedural constraint: the partnership reading is a genuine constitutional principle binding even Parliament''s exercise of legislative power; foreclosure would occur only if Parliament formally repeals all Treaty legislation and constitutional conventions (unlikely). If persuasive only: Parliament retains unrestricted supremacy and the partnership reading is institutionally inert—extractiveness is held in check by political cost, not legal structure. This determines whether the constraint should be classified as tangled_rope (genuine constraint via procedural duty) or piton (apparent constraint maintained by political theater and judicial review, but legally revocable at Parliament''s will).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliament_override_as_foreclosure, conceptual, 'Whether Parliament''s legislative supremacy forecloses the partnership principle as a binding constraint.').

omega_variable(
    settlement_sufficiency_vs_sovereignty,
    'Can monetary settlements and land redress substitute for governance partnership and sovereignty voice, or do they represent a different kind of claim?',
    'Comparative governance: examine iwi that hold large settlements but lack co-governance seats (e.g., iwi whose claims predate the co-governance framework) vs. iwi with co-governance seats but smaller settlements. Do settled iwi report equivalent satisfaction with redress vs. voice? Are there governance outcomes (resource management, policy outcomes) that differ based on voice vs. money?',
    'If substitutable: the partnership reading''s constraint is essentially financial (redistributing settlement resources) with governance consultation as secondary. If distinct: governance partnership is irreducible to settlement, and the constraint''s core function is power-sharing, not redress. This affects how the constraint should be classified: if partnership is reduced to settlements, effective extraction remains very high (Crown controls settlement envelopes, sizes, timing). If partnership includes genuine governance voice, extraction is lower (Māori co-governance dilutes Crown decision-making power). The measurement series assumes partnership includes governance voice; this omega tests that assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_sufficiency_vs_sovereignty, empirical, 'Whether governance partnership is reducible to financial settlement.').

omega_variable(
    collective_iwi_vs_individual_maori_suppression_mechanism,
    'Is the measurement of suppression (0.71) capturing structural (Crown legal barriers, geographic access costs) suppression or internalized (identity-fusion to iwi governance, deference to iwi leadership) suppression?',
    'Post-settlement observation: following settlements, do individual Māori gain independent voice in consultation, or do they remain suppressed by iwi-collective mediation? If settlements remove structural barriers but individuals still defer to iwi governance, suppression is partially internalized. Comparative case: individual Māori in settled vs. unsettled iwi; do they experience equivalent or different suppression after settlement?',
    'If structural: suppression decline from 0.85 to 0.71 is real (barriers removed). If internalized: suppression may be artifactually lower (barriers removed, but deference persists); the constraint''s effective suppression is higher than measured because Māori individuals still lack independent voice. This affects the computed directionality for settlement_beneficiaries: if their suppression is internalized, their effective extraction (χ) may be higher than their direct structural position suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_iwi_vs_individual_maori_suppression_mechanism, empirical, 'Structural vs. internalized suppression in collective settlement framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement_basis(wait_tr_t1840, observed).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement_basis(wait_tr_t1900, observed).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(wait_tr_t1975, observed).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement_basis(wait_tr_t1995, observed).
narrative_ontology:measurement(wait_tr_t2010, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement_basis(wait_tr_t2010, observed).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(wait_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1840, 0.95).
narrative_ontology:measurement_basis(wait_be_t1840, observed).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement_basis(wait_be_t1900, observed).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.88).
narrative_ontology:measurement_basis(wait_be_t1975, observed).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement_basis(wait_be_t1995, observed).
narrative_ontology:measurement(wait_be_t2010, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(wait_be_t2010, observed).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(wait_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1840, 0.85).
narrative_ontology:measurement_basis(wait_su_t1840, observed).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement_basis(wait_su_t1900, observed).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.79).
narrative_ontology:measurement_basis(wait_su_t1975, observed).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.74).
narrative_ontology:measurement_basis(wait_su_t1995, observed).
narrative_ontology:measurement(wait_su_t2010, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement_basis(wait_su_t2010, observed).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(wait_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__partnership_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel produces three structurally distinct constraints corresponding to three readings: (1) crown_sovereignty_reading models the English Article I interpretation (Crown obtained absolute sovereignty, Māori rights are statutory concessions, no ongoing partnership duty); (2) partnership_reading (THIS constraint) models the middle interpretation emerging from courts and political convention (ongoing partnership requiring good faith consultation, settlements, but Parliament retains legislative override); (3) rangatiratanga_reading models the Māori Article II interpretation (Māori retained full authority/tino rangatiratanga, Crown authority is limited to kāwanatanga/governance of settlers). Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different classifications. Crown-sovereignty version has negligible partnership constraint (ε near 0, mountain or piton reading). Partnership-reading (here) is tangled_rope (genuine coordination, asymmetric power). Rangatiratanga-reading is either rope or snare depending on implementation (partnership is genuine; if Crown respects Māori authority, rope; if Crown extracts from hollow consultation, snare). The three constraints are linked via network.affects_constraints: each reading cites the Treaty text and historical debate about the other readings, creating structural dependency. Crown-sovereignty reading uses crown-supremacy axiom; partnership-reading uses good_faith_partnership axiom; rangatiratanga reading uses tino_rangatiratanga_retained axiom. The three are not reducible to one constraint with measurement ambiguity—they have genuinely different ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, powerless, 0.85).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, moderate, 0.45).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
