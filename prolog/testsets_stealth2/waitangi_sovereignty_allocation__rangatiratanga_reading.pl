% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Te Tiriti Article II Rangatiratanga Guarantee (Rangatiratanga Reading)
 *   domain: constitutional/indigenous-rights/post-colonial-governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the
 *   waitangi_sovereignty_allocation kernel: the rangatiratanga_reading, under
 *   which the Maori text of Te Tiriti o Waitangi guaranteed tino
 *   rangatiratanga (full authority) over lands, resources, and taonga while
 *   the Crown gained only kawanatanga (governorship) over its settlers —
 *   sovereignty never ceded. The constraint modeled is that allocation as it
 *   actually stands and operates in Aotearoa's constitutional practice:
 *   partially honored through Waitangi Tribunal jurisprudence, Treaty
 *   settlements, statutory Treaty clauses, and co-governance instruments (Te
 *   Urewera, Te Awa Tupua), partially overridden through parliamentary
 *   supremacy, and currently under organized repudiation pressure. Per the
 *   epsilon-referent rule for kernel readings, epsilon is authored for THIS
 *   standing arrangement — the rangatiratanga guarantee as partially
 *   implemented — assessed by the reading's own lights, NOT for the reading's
 *   endorsed full implementation (which would drive epsilon toward zero
 *   trivially) and NOT for the sibling crown-supremacy arrangement (a
 *   separate file). The claim/metric independence rule is observed:
 *   claimed_type tangled_rope is stated from structural analysis; the metrics
 *   are authored as descriptively true; the engine computes per-seat
 *   classifications and any divergence from the claim is the datum. KEY
 *   AGENTS (by structural relationship): - mana_whenua_iwi_hapu: primary
 *   protected party (organized/identity_locked) — holds the guarantee's
 *   benefit, bears its Crown-mediated costs - crown_government: agenda_setter
 *   (institutional/arbitrage) — administers the guarantee's operation,
 *   collects finality and legitimacy, pays settlements -
 *   settler_descendant_electorate: payer (powerful/constrained) — funds
 *   redress, contests governance dilution, swings enforcement electorally -
 *   waitangi_tribunal: institutional interpreter (institutional/analytical) —
 *   fixes operative meaning without enforcement power -
 *   urban_maori_non_affiliated: marginal payer/beneficiary
 *   (moderate/constrained) — outside settlement capture channels -
 *   taonga_legal_persons: protected non-agent entities (powerless/trapped) —
 *   the guarantee's concrete test cases - international_rights_bodies:
 *   external observer (institutional/analytical, global) — legitimation
 *   pressure without domestic force
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.27).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.22).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.27).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Te Tiriti Article II Rangatiratanga Guarantee (Rangatiratanga Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous-rights/post-colonial-governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '0d014a5b-996e-40e5-80b2-f32460594b2d').
narrative_ontology:cs_kernel_codification('0d014a5b-996e-40e5-80b2-f32460594b2d', fixed_text).
narrative_ontology:cs_authority_grounding('0d014a5b-996e-40e5-80b2-f32460594b2d', lineage).
narrative_ontology:cs_interpretation_layer_present('0d014a5b-996e-40e5-80b2-f32460594b2d').
narrative_ontology:cs_reading_relation('0d014a5b-996e-40e5-80b2-f32460594b2d', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('0d014a5b-996e-40e5-80b2-f32460594b2d', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('0d014a5b-996e-40e5-80b2-f32460594b2d', foundational, maori_sovereignty_never_ceased).
narrative_ontology:cs_axiom_status(maori_sovereignty_never_ceased, holdable).
narrative_ontology:cs_axiom_grounding('0d014a5b-996e-40e5-80b2-f32460594b2d', maori_sovereignty_never_ceased, empirically_contingent).
narrative_ontology:cs_axiom('0d014a5b-996e-40e5-80b2-f32460594b2d', foundational, rangatiratanga_inherent_not_granted).
narrative_ontology:cs_axiom_status(rangatiratanga_inherent_not_granted, holdable).
narrative_ontology:cs_axiom_grounding('0d014a5b-996e-40e5-80b2-f32460594b2d', rangatiratanga_inherent_not_granted, deontological).
narrative_ontology:cs_axiom('0d014a5b-996e-40e5-80b2-f32460594b2d', secondary, crown_authority_limited_to_kawanatanga).
narrative_ontology:cs_axiom_status(crown_authority_limited_to_kawanatanga, holdable).
narrative_ontology:cs_axiom_grounding('0d014a5b-996e-40e5-80b2-f32460594b2d', crown_authority_limited_to_kawanatanga, conventional).
narrative_ontology:cs_reference_frame('0d014a5b-996e-40e5-80b2-f32460594b2d', unceded_rangatiratanga_order).
narrative_ontology:cs_drift_state('0d014a5b-996e-40e5-80b2-f32460594b2d', post_wai1040_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0d014a5b-996e-40e5-80b2-f32460594b2d', '2026-06-12T09:14:00Z').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, mana_whenua_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, mana_whenua_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_descendant_electorate).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_non_affiliated).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_non_affiliated).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_principles_reform_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Iwi and hapū holding mana whenua over traditional territories. They receive negotiated redress: land and asset transfers, co-governance seats over rivers, lakes, and conservation estates, statutory acknowledgment of their authority, and protection of taonga from wahi tapu to te reo Maori. They also carry the arrangement's burdens: redress arrives through Crown-designed processes with capped quantum under 'full and final' settlement deeds, their exercise of authority is codified into statutes Parliament can amend or repeal, and representation channels run through mandated iwi organizations not all hapū recognize. Exit is unavailable in any ordinary sense: whakapapa ties people to place, and moving abroad does not end the relationship to the rohe.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, mana_whenua_iwi_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, mana_whenua_iwi_hapu, payer).

% The executive and Parliament. They administer how the guarantee operates: defining 'Treaty principles,' deciding which claims proceed, setting the fiscal envelope, drafting every implementing statute, and retaining formal power to legislate over any co-governance arrangement. They receive recognized governorship over their settlers, closure of historical grievances through settlement deeds, and international legitimacy; they pay settlement outlays (roughly $2.8 billion since 1989), ceded decision-making discretion, and recurring political contention. Their characteristic move is arbitrage between the two 1840 texts and between readings as electoral coalitions shift.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, beneficiary).

% The general voting public descended largely from settlers and later migrants. They fund redress through taxation, adapt to shared decision-making over water infrastructure and local government, and supply the votes that determine whether enforcement expands or contracts. Many experience the arrangement as unequal citizenship — differential political rights without corresponding accountability — and organize accordingly; others support it as overdue honor of a bargain their ancestors struck. Leaving the polity is possible only by emigration, which few take.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_descendant_electorate, payer,
    powerful, biographical, constrained, national).

% A standing commission of inquiry, half judicial and half historical. It hears claims that Crown action breached the guarantee, takes tikanga and archival evidence, and issues findings that fix what the words mean in practice — including the 2014 Te Paparahi o te Raki finding that northern rangatira did not cede sovereignty in 1840. Its recommendations are mostly non-binding; it shapes meaning and legitimacy but cannot compel compliance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% A political bloc centered on the ACT Party and allied commentators seeking to replace the guarantee's open-textured operation with a short statutory definition of 'principles,' put to referendum. They bear the arrangement's costs as they define them — differential treatment, co-governance without direct electoral accountability — and their strategy is agenda control: coalition agreements, select committee fights, and public persuasion rather than withdrawal from the arrangement.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_principles_reform_coalition, payer,
    organized, biographical, constrained, national).

% Maori living outside their ancestral rohe or unaffiliated with mandated iwi organizations. Redress and co-governance benefits route through iwi corporate structures they may not belong to, so the arrangement's goods reach them weakly, while they share its civic costs and carry its identity stakes fully. Pan-tribal urban authorities fought for recognition through the 1980s and 1990s with only partial success.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_non_affiliated, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_non_affiliated, beneficiary).

% Rivers, mountains, and sites vested with legal personhood — Te Urewera, Te Awa Tupua (Whanganui River) — represented by appointed guardian bodies combining iwi and Crown appointees. They cannot act or speak except through their guardians; their statutory standing is the arrangement's most concrete institutional innovation and its most cited precedent for elsewhere.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, taonga_legal_persons, beneficiary,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__rangatiratanga_reading, taonga_legal_persons).

% UN treaty bodies and the Expert Mechanism on the Rights of Indigenous Peoples reviewing Aotearoa's performance against the UN Declaration on the Rights of Indigenous Peoples. They lend external legitimation to the guarantee's claims and criticize rollbacks, wielding moral and reputational rather than domestic legal force.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders the relationship between two polities occupying one territory: Maori full authority over their lands, waters, and treasures; Crown government over its settlers. It fixes who decides what, provides a grievance channel when the boundary is crossed, and enables joint management of shared resources without merging the two authorities.
% TRANSFER_FUNCTION: Moves decision rights, land, and money: settlement deeds transfer assets and cash from the Crown to iwi governance bodies; co-governance statutes move seats and veto points from exclusive Crown control to shared bodies; the burden of justifying actions affecting taonga shifts onto the Crown. In return, claim-finality and political legitimacy flow to the Crown.
% ABSENT_VOICES: Rank-and-file hapū members outside mandated iwi organizations, urban Maori unaffiliated with settlement structures, and ordinary taxpayers and ratepayers are absent from the negotiating tables — settlements are struck between Crown negotiators and iwi leadership, and the wider publics of both polities ratify outcomes after the fact. Future generations of both polities hold stakes no seated agent represents.
% DISAPPEARANCE_RATIONALE: If the guarantee's operative force vanished overnight — every Treaty clause, settlement obligation, and co-governance statute repealed — Maori-Crown relations would revert to raw demographic majority rule with no grievance channel; settled claims would reopen; taonga protections including legal personhood and co-management would lapse; and the legitimacy bargain underlying the state's title to much of its estate would collapse into open dispute. Both polities' current arrangements presuppose the guarantee; its removal forces rearrangement, not equilibrium.
% FOUNDING_PROBLEM: After Britain recognized Maori independence in He Whakaputanga (1835) and settlement accelerated, two authorities faced each other in one territory. The Treaty was struck to secure both: Maori authority over their lands and treasures, British order over its growing settler population — with the Maori text recording that neither side ceded to the other what this reading says neither ceded.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: Colonial Office records of Britain's 1835 recognition of He Whakaputanga; Paakehaa constitutional historiography of the two texts' divergence (Claudia Orange's textual studies); and the Waitangi Tribunal's Te Paparahi o te Raki inquiry (2014), a Crown-created body whose multi-year evidentiary process heard both texts and found northern rangatira did not cede sovereignty. The live dispute itself — crown-sovereignty parties contesting this reading — corroborates that the founding problem remains unresolved rather than settled.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.27, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.27: from this reading's own lights the allocation's core content is protective — it secures authority the reading holds was never surrendered — but its actual operation routes through Crown-controlled machinery (capped 'full and final' settlements, revocable co-governance statutes, Crown-defined 'principles') that converts inherent, perpetual authority into conditional, statutorily revocable packages. That conversion gap is the extractive residue; it is real but modest against the protective core. Suppression 0.22: the constraint coerces little — it is if anything under-enforced; its binding force on Maori runs through settlement-deed finality and Crown vetoes, and it cannot compel the Crown at all. Theater 0.42: a substantial performative layer (ceremonial acknowledgment, principles rhetoric, consultation without consent power) coexists with genuinely functional cores (Tribunal jurisprudence, real asset transfers, co-governance bodies exercising actual decisions). Accessibility_collapse 0.30: understanding the rangatiratanga claim does not collapse alternatives — the crown-sovereignty and partnership readings remain fully live and politically dominant, so alternatives persist. Resistance 0.68: the constraint meets sustained organized resistance (Treaty Principles Bill, coalition review commitments, local-government pushback, and the historical arc of armed and legislative resistance). The temporal series run on ONE shared eight-point grid (1840-2025) with all three metrics authored at every point. Trajectory: extraction peaked around 1900 (Land Wars aftermath, Native Land Court individualization dismantling rangatiratanga under the guarantee's nominal cover) and decays as enforcement matured, with a 2004 uptick (Foreshore and Seabed Act overriding judicial recognition) and a current dip. Suppression_requirement traces the constraint's active force against Crown unilateralism: dormant to 1975, ratcheting through the Tribunal's 1985 retrospective jurisdiction, the settlement machinery, and co-governance statutes, dipping recently under repudiation pressure. Note the distinction: base_properties.suppression (0.22) measures coercion the constraint exerts on governed agents; the suppression_requirement series measures the constraint's suppressive force against Crown override as enforcement capacity built and now erodes — different constructs, both authored. Extractiveness is scaled by directionality and scope in the engine's computation; suppression is not scaled.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the crown_government seat (agenda_setter, arbitrage exit) the guarantee is a manageable cost paired with a legitimacy purchase — low-extraction coordination it administers at will. From the settler_descendant_electorate seat (payer, powerful, constrained) the same structure operates as democratic dilution — extraction from majoritarian self-rule. From the mana_whenua seat (beneficiary, identity_locked) the salient fact is neither: it is the gap between an inviolable guarantee and its capped, revocable delivery — under-delivery, not over-extraction. The Tribunal seat sees an interpretive contest; the international seat sees a compliance record. The engine computes these per-seat classifications from power, exit, and role; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. mana_whenua_iwi_hapu sit at the beneficiary pole (d near 0.0): the allocation subsidizes and protects them, and their identity_locked exit (whakapapa-constituted — the relationship to the rohe constitutes the self, so exit is unthinkable rather than blocked) pins them there; their cost-bearing arises from breach and Crown mediation, not from the allocation itself, so no upward override is authored. settler_descendant_electorate derive high d (~0.7) as victims with powerful-but-constrained position: they bear taxation and ceded decision rights and cannot exit the polity. One override is declared: crown_government, d_value 0.30. Listed under beneficiaries, the derivation would place the Crown near the pure-beneficiary pole (~0.1); but the Crown demonstrably bears costs through the same structure — settlement outlays, ceded discretion, political contention — against gains of finality, legitimacy, and recognized kawanatanga, netting to near-symmetric with a beneficiary tilt. The override is keyed to the institutional power atom, which is coarse: it also touches the Tribunal and international bodies, but both are observers whose chi contribution is negligible, so the coarseness is tolerable. Identity-lock dynamics: the lock is relational and constitutive (whakapapa), not professional or ideological; if urbanization and generational change weakened it, mana_whenua directionality would drift toward symmetric and the classification could soften toward rope — tracked in omega whakapapa_identity_lock_persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two authorities, one territory — is live, so no mandatrophy is declared. The classification guards against mislabeling in both directions. Reading the allocation as pure rope would erase the asymmetric extraction the reading itself identifies: Crown-mediated machinery converting inherent authority into revocable packages, visible transfers landing alongside deeper structural gains accruing to the Crown. Reading it as snare would erase the genuine coordination function — dual-polity boundary-setting, grievance channel, taonga co-management — and would contradict this reading's own assessment that the allocation protects. Tangled_rope holds both faces: real coordination AND asymmetric cost-bearing through the same structure, held together by active enforcement (nothing in the arrangement is self-executing; every gain exists at Parliament's pleasure and must be continuously defended through litigation, settlement deeds, and statute). The constraint is not a piton: its function has not atrophied — transfers are real, seats are real, jurisprudence accumulates — though the current rollback pressure tests whether enforcement decay could begin a piton trajectory (omega enforcement_rollback_cyclicity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the waitangi_sovereignty_allocation kernel; what classifications would the sibling readings assign to their own instantiations, and how does the per-seat structure invert across readings?',
    'Generate and compare the sibling files (crown_sovereignty_reading, partnership_reading): identical referent texts, reading-indexed epsilon, inverted or shifted beneficiary/victim sets; compare per-seat outputs across the family.',
    'Under the crown reading the payer/beneficiary sets invert and epsilon rises sharply from this seat''s lights; under the partnership reading extraction sits intermediate. Cross-reading comparison, not within-story hedging, resolves the indexicality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of one kernel; sibling files carry the other readings.').

omega_variable(
    ceding_locus_disagreement,
    'Did Article I effect a cession of sovereignty (English text) or a grant of kawanatanga over settlers only, with Article II of the Maori text guaranteeing tino rangatiratanga — and which text governs where they diverge?',
    'Philological reconstruction of the 1840 texts, contemporaneous oral testimony and explanations given to rangatira, and the Tribunal''s WAI 1040 evidentiary method comparing what was said against what was drafted.',
    'Resolves which reading''s constraint binds at the kernel level: a cession finding dissolves this reading''s foundational premise; a no-cession finding dissolves the crown reading''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceding_locus_disagreement, empirical, 'The locus of the kernel contest: the effect and meaning of Article I across the two texts.').

omega_variable(
    inherent_vs_treaty_constituted_authority,
    'Is rangatiratanga inherent — pre-existing, He Whakaputanga-backed, surviving the Treaty regardless of Crown conduct — or constituted by the Treaty''s guarantee?',
    'Constitutional-theoretical analysis plus the 1835 recognition record; operational test: would rangatiratanga''s authority survive formal repudiation of the Treaty itself?',
    'If inherent, the claim''s authority precedes and survives Crown repudiation attempts (mountain-like persistence of the claim, though not of its enforcement); if Treaty-constituted, the Crown holds a modification path and the constraint''s persistence depends entirely on enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_treaty_constituted_authority, conceptual, 'Source of the authority this allocation allocates — prior fact or treaty grant.').

omega_variable(
    allocation_vs_machinery_extraction,
    'Is the measured extraction (epsilon 0.27) a property of the rangatiratanga allocation itself, or of the Crown-mediated machinery that implements it (capped settlements, revocable statutes, Crown-defined principles doctrine)?',
    'Decompose and compare: author a separate story for the settlement machinery and contrast epsilon; examine co-governance instruments that bypass Crown mediation (direct statutory vesting such as Te Urewera) for systematically lower extraction.',
    'If machinery-borne, the allocation''s true epsilon falls toward rope levels and the machinery file carries the extraction; if allocation-borne, the tangled_rope reading deepens toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_vs_machinery_extraction, empirical, 'Locating the extraction: allocation core versus implementation machinery.').

omega_variable(
    whakapapa_identity_lock_persistence,
    'How durable is the identity lock pinning mana whenua directionality near the beneficiary pole — do urbanization, trans-Tasman migration, or generational change weaken whakapapa-constituted exit-trapping?',
    'Longitudinal cohort data on Maori identification, affiliation with iwi structures, and engagement with settlement and co-governance institutions across generations.',
    'If the lock weakens, directionality shifts toward symmetric, payer-side costs weigh more heavily, and the classification could drift toward rope; if it holds, the beneficiary-pole d persists across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whakapapa_identity_lock_persistence, empirical, 'Durability of the identity-fusion mechanism behind exit trapping.').

omega_variable(
    enforcement_rollback_cyclicity,
    'Is the 2023-2025 repudiation pressure (Treaty Principles Bill, coalition review commitments) a permanent enforcement-capacity step-down, or one phase of a recurrent cycle of retrenchment and restoration?',
    'Track the Bill''s fate, replacement statutes, and settlement/co-governance continuity across electoral cycles; extend the suppression_requirement series beyond 2025 on the same grid.',
    'A permanent step-down dates a transition toward piton — theatrical maintenance of a hollowed guarantee; a cyclical resolution keeps the tangled_rope classification with oscillating enforcement intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_rollback_cyclicity, empirical, 'Whether current rollback is trend or cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.32).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1975, 0.5).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1985, 0.46).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1995, 0.41).
narrative_ontology:measurement(wait_tr_t2004, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2004, 0.52).
narrative_ontology:measurement(wait_tr_t2014, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1975, 0.66).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement(wait_be_t2004, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2004, 0.49).
narrative_ontology:measurement(wait_be_t2014, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2014, 0.37).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2025, 0.27).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.05).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.03).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1975, 0.16).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1985, 0.31).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1995, 0.46).
narrative_ontology:measurement(wait_su_t2004, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2004, 0.52).
narrative_ontology:measurement(wait_su_t2014, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Treaty of Waitangi' covers three structurally distinct sovereignty allocations. Per the epsilon-invariance principle the kernel decomposes into three constraint stories over the same referent texts: this file authors the rangatiratanga allocation (epsilon 0.27 from its own lights — protective core with Crown-mediated extraction residue); the crown_sovereignty file authors complete cession and Westminster supremacy (very high epsilon from this seat's lights); the partnership file authors the good-faith middle (intermediate). Family links run through network.affects_constraints. Upstream/downstream structure: the 1840 textual record feeds all three; rangatiratanga jurisprudence exerts downstream pressure on the partnership reading by raising what 'active protection' must deliver, while the crown reading's parliamentary supremacy is the enforcement environment all three operate within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
