% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'territorial_legitimacy': the indigenous_continuity reading asserts that
 *   Palestinian territorial legitimacy derives from continuous indigenous
 *   habitation prior to 1948, that the Israeli state constitutes a
 *   settler-colonial entity illegally occupying Palestinian land, that the
 *   Nakba (the Palestinian catastrophe of 1948) was a displacement rather
 *   than a partition, and that the right of return for Palestinian refugees
 *   and their descendants is structurally central to legitimate territorial
 *   settlement. This reading frames 1948 not as the birth of a recognized
 *   state but as the violent dispossession of an indigenous population. The
 *   reading competes within the same kernel against the partition_reading
 *   (legitimacy via UN Resolution 181 borders and international recognition)
 *   and the security_necessity_reading (legitimacy via defensive territorial
 *   control and 1967 borders). The constraint operates through institutional
 *   suppression of the reading's claims in Israeli law, international
 *   diplomatic forums, and Palestinian Authority governance structures that
 *   have negotiated away full sovereignty claims.
 *
 * KEY AGENTS:
 *   - Palestinian National Movement: Claims indigenous continuity, frames 1948 as Nakba, demands right of return, asserts sole legitimate sovereignty over all historic Palestine
 *   - Palestinian Refugees (1948 onwards) & Diaspora: Constitute both the structural evidence of dispossession (Nakba) and the primary victims of the constraint's enforcement (right of return denied)
 *   - Israeli State & Institutional Structure: Enforces the partition-reading legitimacy; legally erases Palestinian presence; prevents refugee return; maintains settler institutions
 *   - International Legal Authorities: Oscillate between recognizing partition legitimacy and acknowledging right-of-return principles; provide no enforcement mechanism for this reading
 *   - Palestinian Authority Governance: Partially suppresses this reading by negotiating borders, accepting compensation-over-return frameworks, managing refugee camps under constraints
 *   - Academic & Historiographical Community: Documents Palestinian continuity, Nakba evidence, settler-colonial dynamics; contested consensus on historical narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.88).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.91).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Indigenous Continuity and Anti-Colonial Self-Determination").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'ebf4ee11-a444-46b3-a180-2e292faa18bb').
narrative_ontology:cs_kernel_codification('ebf4ee11-a444-46b3-a180-2e292faa18bb', fixed_text).
narrative_ontology:cs_authority_grounding('ebf4ee11-a444-46b3-a180-2e292faa18bb', extraction).
narrative_ontology:cs_interpretation_layer_present('ebf4ee11-a444-46b3-a180-2e292faa18bb').
narrative_ontology:cs_reading_relation('ebf4ee11-a444-46b3-a180-2e292faa18bb', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('ebf4ee11-a444-46b3-a180-2e292faa18bb', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('ebf4ee11-a444-46b3-a180-2e292faa18bb', foundational, indigenous_palestinian_territorial_primacy).
narrative_ontology:cs_axiom_status(indigenous_palestinian_territorial_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ebf4ee11-a444-46b3-a180-2e292faa18bb', indigenous_palestinian_territorial_primacy, deontological).
narrative_ontology:cs_axiom('ebf4ee11-a444-46b3-a180-2e292faa18bb', foundational, settler_colonialism_delegitimizes_occupation).
narrative_ontology:cs_axiom_status(settler_colonialism_delegitimizes_occupation, holdable).
narrative_ontology:cs_axiom_grounding('ebf4ee11-a444-46b3-a180-2e292faa18bb', settler_colonialism_delegitimizes_occupation, deontological).
narrative_ontology:cs_axiom('ebf4ee11-a444-46b3-a180-2e292faa18bb', secondary, right_of_return_non_negotiable_restitution).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable_restitution, holdable).
narrative_ontology:cs_axiom_grounding('ebf4ee11-a444-46b3-a180-2e292faa18bb', right_of_return_non_negotiable_restitution, deontological).
narrative_ontology:cs_reference_frame('ebf4ee11-a444-46b3-a180-2e292faa18bb', indigenous_palestinian_continuity_pre_colonial_presence).
narrative_ontology:cs_drift_state('ebf4ee11-a444-46b3-a180-2e292faa18bb', contemporary_2024, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('ebf4ee11-a444-46b3-a180-2e292faa18bb', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948_onwards).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, internally_displaced_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, stateless_palestinians_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948_onwards).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, historiographical_and_academic_communities).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_authority_governance).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_rights_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, right_of_return_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and articulates Palestinian indigenous continuity, demands recognition of the Nakba as dispossession rather than partition, organizes around right of return and full Palestinian sovereignty. The movement carries Palestinian collective identity and the foundational narrative of continuous presence and anti-colonial struggle. Exit from this claim would constitute identity dissolution for the movement itself—it is locked into the reading by its constitutive purpose. Benefits from the reading by maintaining collective identity and delegitimizing Israeli state claims, though the actual enforcement of the reading (territorial control, return rights, sovereignty) remains blocked.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_movement, beneficiary,
    organized, generational, identity_locked, continental).

% Constitute the primary evidence of the Nakba and the primary victims of territorial dispossession. Denied return to ancestral homes, confined to refugee camps or diaspora, legally stateless in many host countries. The reading claims them—right of return is structurally central to the constraint. They benefit from the reading's recognition of their dispossession and restoration claim, but pay through indefinite statelessness, legal marginalization, economic precarity, and the enforced denial of return. Trapped by international refugee law frameworks and host country restrictions; identity-locked by Palestinian nationality despite having no state.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948_onwards, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_1948_onwards, beneficiary).

% Displaced from homes within what became Israel (1948) or within Palestinian territories (1967 onward). Legally confined to zones (Gaza, West Bank enclaves, Arab-Israeli villages under demographic threat). Lack citizenship equality even in nominally shared Israeli state. Bear the constraint through legal restrictions, settlement pressures, home demolitions, and zoning that prevents return to original properties. Trapped by military occupation, Israeli legal structures, and Palestinian Authority administrative divisions. The reading claims them as indigenous dispossessed; the constraint denies them return and full sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, internally_displaced_palestinians, payer,
    powerless, biographical, trapped, national).

% Second and third-generation Palestinians in diaspora (Lebanon, Syria, Jordan, beyond) who inherited statelessness from parent generation. Legally precarious in host countries, unable to return to Palestine under current territorial regime, unable to naturalize fully in diaspora states. Carry Palestinian collective identity but lack institutional representation or territorial anchor. The reading frames them as holders of the right of return; the constraint denies return and locks them into perpetual diaspora status.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, stateless_palestinians_diaspora, payer,
    powerless, generational, constrained, global).

% Maintains the institutional suppression of the indigenous_continuity reading through law, policy, and enforcement: citizenship law denies Palestinian return; settlement law and zoning prevent Palestinian territorial claims; educational curricula teach partition-based legitimacy; security apparatus enforces demographic boundaries. The reading directly challenges Israeli legitimacy by asserting settler-colonialism; Israeli institutions suppress it through legal erasure, security measures, and institutional denial. Benefits from the suppression by maintaining territorial control, Jewish demographic majority, and uncontested statehood. Exit from suppression would mean accepting indigenous claims and returning territorial control and refugee populations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Nominally represents Palestinians but operates under Israeli security coordination and international aid conditioning that requires abandoning full sovereignty claims and right of return. Administers refugee camps under international mandate. Negotiates borders and compensation frameworks that de-prioritize the indigenous_continuity reading. Partially enforces the constraint against the reading by suppressing its institutional articulation, managing refugee populations as technical problems rather than political claimants, and directing Palestinian resources toward state-building within partition-accepted borders. Sits in asymmetry: claims to represent Palestinians while enforcing constraints against Palestinian foundational claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_authority_governance, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_authority_governance, payer).

% UN bodies, international courts, humanitarian organizations, and legal institutions that hold both refugee-return principles and state recognition frameworks. Theoretically acknowledge right-of-return in principle (UNGA resolutions) while practically recognizing Israel as a legitimate state (UN membership). This institutional contradiction creates the constraint: acknowledge the reading but do not enforce it. Maintain partition-based borders and state stability as practical operational logic while preserving rhetorical commitment to refugee rights. Sit in tension rather than as pure beneficiaries or payers.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_authorities, observer,
    institutional, generational, analytical, global).

% Document Palestinian continuity, indigenous presence pre-1948, Nakba evidence, oral histories, and settler-colonial dynamics. Produce the historical and theoretical work that sustains the indigenous_continuity reading. Benefit from the reading's adoption through professional recognition, publication, and intellectual authority. Sit outside the direct enforcement apparatus but provide the knowledge infrastructure that legitimizes or delegitimizes the reading depending on institutional context.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, historiographical_and_academic_communities, beneficiary,
    moderate, generational, mobile, global).

% Secular Palestinian movements, marxist parties, and secular nationalist organizations that articulated the indigenous_continuity reading most forcefully in the 1960s-1980s (PFLP, DFLP, secular PLO factions). Increasingly sidelined by Palestinian Authority institutionalization, Israeli security measures targeting leftist movements, and the rise of Islamist organizations. Would advocate for the reading if in decision-making forums; excluded from institutional representation. Their absence from official Palestinian governance structures weakens the reading's institutional articulation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, excluded_palestinian_left_movements, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None that this reading acknowledges as legitimate coordination. The reading frames the constraint as pure dispossession and occupation, not as solving a coordination problem. (The partition_reading and security_necessity_reading may identify coordination functions—international order, refugee management, security provision—but this reading rejects those as rationalizations for illegitimate territorial seizure.)
% TRANSFER_FUNCTION: Transfers Palestinian territorial sovereignty, demographic majority, return rights, and political self-determination to the Israeli state and international recognition system. Moves Palestinian refugees and internally displaced persons into perpetual statelessness, legal marginalization, and diaspora confinement. Extracts legitimacy from Palestinian indigenous presence and confers it on the Israeli state, which frames itself as a legitimate partition-based polity rather than a settler-colonial entity.
% ABSENT_VOICES: Palestinian refugees and diaspora populations are formally absent from Israeli governance and Palestinian Authority decision-making despite being the primary victims and primary holders of the right-of-return claim. Secular Palestinian leftist movements are institutionally sidelined by Palestinian Authority dominance and Israeli security suppression. International legal authorities that acknowledge refugee-return principles remain absent from enforcement mechanisms. Academic historians documenting indigenous Palestinian presence are marginalized in Israeli public discourse and constrained in Palestinian institutional forums by security and political pressures.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight—if indigenous Palestinian claims were institutionally recognized, if the right of return were operationalized, if Palestinian sovereignty over all of historic Palestine were established—the territorial, political, and demographic landscape would be fundamentally reorganized. Israeli state borders would be redrawn or eliminated; refugee populations would return; institutional structures would shift from Israeli dominance to Palestinian majority governance; international recognition frameworks would reorient toward Palestinian statehood; the legal and security systems that currently enforce demographic and territorial control would be dismantled. The world does not rearrange toward a natural equilibrium; it rearranges toward a radically different state system and territorial allocation.
% FOUNDING_PROBLEM: Palestinian indigenous population was continuously present on the land of Palestine prior to and during the colonial period. At the moment of the Nakba (1948), Palestinians were violently dispossessed of homes, property, and territorial control by a state claiming legitimacy through partition and international recognition, despite being constituted by settler-colonial migration. The founding problem is: how do dispossessed indigenous populations reclaim territorial sovereignty, return rights, and recognition against states built on their dispossession?
% FOUNDING_PROBLEM_CORROBORATION: Attested by Palestinian national movements, refugee organizations, and international human-rights bodies (UN Office for Coordination of Humanitarian Affairs documents displacement). Corroborated by academic historians (Ilan Pappé, Rashid Khalidi, Nur Masalha) documenting pre-1948 Palestinian presence, village records, and forced displacement. Affirmed by international legal authorities acknowledging UNGA Resolution 194 right-of-return principle. Contested by Israeli institutional narratives (partition legitimacy, security necessity) and mainstream international legal consensus (state recognition). The founding problem persists as a live claim for Palestinian movements and diaspora; its non-resolution is the measure of the constraint's mandatrophy.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the constraint structurally denies Palestinian sovereignty, territorial control, and refugee return—the foundational restitution demands. Suppression is higher still (0.91) because the constraint's persistence depends on active institutional erasure: Israeli law delegitimizes Palestinian claims; international law frameworks privilege partition recognition over indigenous restitution; Palestinian leadership is incentivized to abandon the reading via aid conditioning and border control. Theater is moderate (0.42): the reading itself is carried by historians, legal theorists, and social movements, but mainstream diplomatic and legal forums treat it as a negotiating position rather than a legitimate founding claim. Accessibility of alternatives collapses substantially (0.79) once the reading is adopted: accepting it forecloses the partition framework's borders, security logic, and international recognition path—the reading demands total reorientation of legitimacy foundations. Resistance is substantial (0.73): the reading meets continuous institutional denial from Israeli authorities, Palestinian leadership compromises, and international legal structures that privilege the partition frame. The measurement series track increasing extraction over 32 time units (roughly 1948-2020 normalized): as Israeli settlement expands, as Palestinian Authority negotiating power weakens, as refugee return remains structurally blocked, the extractive force accumulates. Theater rises (28% to 42%) because increasingly, this reading is invoked rhetorically by popular movements while institutional negotiation abandons it—the gap between claim and implementation widens. Suppression rises (81% to 91%) as enforcement mechanisms harden: legal barriers, demographic engineering, normalization agreements, and PA administrative suppression of return-demand rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian movement seat: the constraint is a snare—foundational restitution rights are structurally denied through institutional violence (displacement, law, borders, demographic engineering). From the Israeli state seat: the same institutional structure is read as legitimate partition-based statehood with security necessity overlays; the right-of-return is reframed as a negotiating demand rather than a legal claim. From international authority seat: the constraint appears as tangled_rope—genuine coordination problems (refugee management, state stability, international order) are solved via the partition frame, but at asymmetric cost to Palestinian refugees and internally displaced persons. This gap is not a failure of analysis; it is the core structural fact the constraint embeds: the same institutions (borders, refugee camps, demographic law, international recognition) operate as legitimate state-building from one seat and as dispossession from another. The institutional suppression of this reading (via legal erasure, diplomatic marginalization, PA co-optation) is what prevents the perspectival gap from resolving into negotiated settlement.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian national movement and refugees/diaspora sit at the target end (d near 1.0): the constraint structurally denies them territorial sovereignty, return rights, and political recognition—they bear maximal extraction. Israeli institutional structure sits at the beneficiary end (d near 0.0): the constraint legitimizes Israeli statehood despite contested origins, secures Jewish-majority demographics, and prevents territorial loss. International legal authorities sit near symmetric (d ~0.5): they benefit from a system that privileges state recognition and stability (partition frame), but are constrained by rights-based law that acknowledges refugee claims and indigenous self-determination. Palestinian Authority governance sits in asymmetry (d ~0.65): formally representing Palestinians but administratively enforcing the constraint via compromise positions, rendering it partially beneficiary-adjacent while nominally payer-side. The engine will compute per-seat classifications: from the Palestinian movement seat, this is a pure snare; from Israeli institutional seat, this is reframed as legitimate partition-based statehood (the engine will read the contested claim); from international authority seat, it is tangled_rope (coordination of state system + asymmetric refugee exclusion). The divergence IS the measurement the corpus takes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is stated as Palestinian self-determination against colonial occupation (live). The constraint's mandate is to exclude Palestinians from territorial sovereignty and return (still operationalized). The founding problem and the constraint are INVERTED: the constraint exists to prevent the problem's solution, not to solve it. This is the signature of mandatrophy—a mandate that persists by preventing its own founding problem's resolution. The measure of mandatrophy is: can you state a world in which the founding problem is solved? Yes: Palestinian sovereignty, right of return, recognition of indigenous continuity. Is the constraint necessary for that solution? No—the constraint is an obstacle to it. Does the constraint persist because the founding problem persists? No—it persists because institutional power locks it in place. This reading thus carries severe mandatrophy: the constraint's persistence is not justified by unsolved coordination problems; it is justified by power asymmetry and the institutional suppression of claims. The theater ratio rising (28% to 42%) signals performative maintenance: invocations of the reading in political speech without institutional implementation, while suppression mechanisms intensify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_presence_measurement,
    'What constitutes sufficient ''continuous indigenous habitation'' to ground territorial legitimacy—and how is continuity measured across displacement, diaspora, legal erasure, and generational break?',
    'Historiographical consensus on Palestinian presence and institutional continuity pre-1948; anthropological documentation of kinship, land records, oral tradition transmission, and demographic reconstruction across the Nakba rupture.',
    'If continuity is interpreted narrowly (unbroken physical presence only), the constraint''s core claim weakens under the lived reality of dispersal. If interpreted broadly (genealogical, juridical, cultural transmission), it strengthens even across displacement—which is itself the reading''s central claim about anti-colonial restitution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_presence_measurement, conceptual, 'The measurement standard for ''indigenous continuity'' across catastrophic displacement.').

omega_variable(
    settler_colonial_classification,
    'Is the Israeli state structurally a settler-colonial entity (competing populations with incompatible territorial claims rooted in origin narratives), or a post-colonial state whose legitimacy rests on international recognition despite contested history?',
    'Comparative historical-institutional analysis: does the Israeli state''s founding narrative, legal structure, settlement policy, and demographic engineering match the settler-colonial template (extraction, replacement, legitimacy via origin claim and security-necessity framing) or a post-1945 state recognition framework (sovereignty via international law regardless of origin)?',
    'If settler-colonial classification holds, the reading''s entire extraction logic flows—the constraint is structurally a snare disguised as security necessity. If contested (a hybrid form, or a state legitimated by 1948 partition law despite contested origins), the constraint becomes tangled_rope or scaffold with a more complex beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_classification, conceptual, 'Whether Israeli statehood is classified as settler-colonialism or post-partition sovereignty.').

omega_variable(
    right_of_return_operationalization,
    'How is the ''right of return'' operationalized—full repatriation of all refugees and descendants, compensation without return, territorial autonomy with limited return, or symbolic return combined with reparations?',
    'Palestinian political consensus on the right''s scope; negotiated settlements that specify return pathways; international legal precedent from other post-conflict refugee situations (Bosnia, Armenia, etc.); empirical modeling of demographic outcomes under different return scenarios.',
    'Narrow operationalization (compensation only) transforms the reading from foundational restitution claim to a material-transfer constraint—shifting ε downward and potentially reclassifying from snare toward tangled_rope. Broad operationalization (right of return with majority-muslim demographic outcome) hardens the constraint as structurally incompatible with Israeli-Jewish majority statehood and locks in snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_operationalization, empirical, 'The scope and mechanism of right-of-return implementation.').

omega_variable(
    committer_frame_sibling_coexistence,
    'Can this reading (indigenous_continuity) coexist within a single authoritative framework with the partition_reading, or does the assertion of Palestinian sovereignty over all historic Palestine logically foreclose the legitimacy of the 1948 partition borders?',
    'Examination of whether the two readings can be held as interpretations of the same kernel (contested legitimacy basis) by different parties in active dispute, or whether they represent mutually exclusive foundational claims that cannot both be true within one coherent authority structure.',
    'If coexistence is maintained (different parties hold different readings; dispute is ongoing without foreclosure), the constraint is a live reading in a distributed-authority system. If this reading forecloses partition_reading (total Palestinian sovereignty admits no 1948 border legitimacy), the classification shifts toward pure repudiation and the constraint''s persistence depends entirely on suppression and institutional capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_coexistence, conceptual, 'Whether this reading structurally forecloses or coexists with the partition reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(terr_tr_t8, observed).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(terr_tr_t16, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(terr_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement_basis(terr_be_t8, observed).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 16, 0.81).
narrative_ontology:measurement_basis(terr_be_t16, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 24, 0.85).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 32, 0.88).
narrative_ontology:measurement_basis(terr_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement_basis(terr_su_t8, observed).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 16, 0.87).
narrative_ontology:measurement_basis(terr_su_t16, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 32, 0.91).
narrative_ontology:measurement_basis(terr_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__indigenous_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel decomposes into three constraint stories corresponding to three distinct readings of legitimacy grounds: (1) indigenous_continuity_reading: Palestinian legitimacy via anti-colonial self-determination and continuous habitation; (2) partition_reading: legitimacy via international legal partition and state recognition; (3) security_necessity_reading: legitimacy via defensive territorial control and security. Each reading has its own ε (extractiveness), beneficiary/victim structure, and classification. They are linked as a constraint family because each reading's dominance affects the others' operative scope and credibility. This story (indigenous_continuity_reading) influences both siblings by establishing the historical claim that the partition was a dispossession rather than a neutral division. The sibling stories in turn foreclose or coexist with this reading depending on the authority structure and institutional framework examined.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
