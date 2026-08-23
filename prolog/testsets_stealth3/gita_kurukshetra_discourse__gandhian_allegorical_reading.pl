% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of the Kurukshetra Discourse: the Standing Caste-and-War Deployment Indicted
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The kernel is the Bhagavad Gita's Kurukshetra discourse; this story
 *   instantiates the Gandhian allegorical reading, and the arrangement it is
 *   ABOUT — its epsilon referent — is the standing deployment of the text as
 *   a divine charter of varna duty and righteous violence, administered by a
 *   hereditary interpreter class. From this reading's seat that deployment is
 *   a structure in which the vocabulary of dharma and cosmic order wraps
 *   compulsory hereditary labor, untouchability, and sanctified war, held in
 *   place by sin-doctrine and an interpretive monopoly; its victims are those
 *   who bore the labor, the exclusion, and the battles. The metrics describe
 *   the deployment's operation as the historical record shows it; the claimed
 *   type records this reading's structural judgment of it. Family note per
 *   the epsilon-invariance principle: the colloquial label 'what the Gita
 *   teaches' decomposes into three files — this one authors high epsilon over
 *   the standing deployment; the orthodox-literal sibling authors low epsilon
 *   over the same referent (it sees legitimate duty coordination); the
 *   universalist-devotional sibling authors an intermediate value (access
 *   opened, divine command retained). The deltas are documented in omega
 *   gita_kernel_reading_delta.
 *
 * KEY AGENTS:
 *   - brahminical_interpreter_class: agenda-setting beneficiary (institutional/arbitrage) — runs the interpretive machinery and collects its returns
 *   - kshatriya_warrior_rulers: primary beneficiary (powerful/constrained) — receives consecration of rule and war
 *   - twice_born_upper_castes: secondary beneficiary and payer (organized/identity_locked) — rank holders who also finance and police the order
 *   - shudra_service_castes: primary target (powerless/trapped) — hereditary labor bound by sin-doctrine
 *   - dalit_outcaste_communities: primary target (powerless/trapped) — exclusion enforced at the body
 *   - dharmic_war_soldiers: primary target (moderate/trapped) — sanctified compulsion to kill and die
 *   - gandhian_allegorical_readers: excluded critics (organized/mobile) — carriers of this reading, outside the authorized interpretive conversation for most of the interval
 *   - heterodox_bhakti_dissenters: excluded objectors (organized/mobile) — the standing dissent record
 *   - modern_gita_hermeneuts: analytical observers (analytical/analytical) — supply the philological and reception-history record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.82).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, snare).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Kurukshetra Discourse: the Standing Caste-and-War Deployment Indicted").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '6daa47da-1aa1-4a16-bdc4-1fdd1f87a209').
narrative_ontology:cs_kernel_codification('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', fixed_text).
narrative_ontology:cs_authority_grounding('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', extraction).
narrative_ontology:cs_interpretation_layer_present('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209').
narrative_ontology:cs_reading_relation('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', foundational, kurukshetra_internal_battlefield).
narrative_ontology:cs_axiom_status(kurukshetra_internal_battlefield, holdable).
narrative_ontology:cs_axiom_grounding('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', kurukshetra_internal_battlefield, deontological).
narrative_ontology:cs_axiom('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', foundational, ahimsa_supreme_principle).
narrative_ontology:cs_axiom_status(ahimsa_supreme_principle, holdable).
narrative_ontology:cs_axiom_grounding('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', ahimsa_supreme_principle, deontological).
narrative_ontology:cs_axiom('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', secondary, individual_conscience_over_clerical_authority).
narrative_ontology:cs_axiom_status(individual_conscience_over_clerical_authority, holdable).
narrative_ontology:cs_axiom_grounding('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', individual_conscience_over_clerical_authority, conventional).
narrative_ontology:cs_reference_frame('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', ahimsa_centered_inner_struggle_frame).
narrative_ontology:cs_drift_state('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', contemporary_martial_nationalism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6daa47da-1aa1-4a16-bdc4-1fdd1f87a209', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreter_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_rulers).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, twice_born_upper_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, shudra_service_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_outcaste_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, dharmic_war_soldiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, twice_born_upper_castes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a hereditary monopoly on Sanskrit exegesis of the Gita and the dharma corpus. Certifies what duty requires of each varna, adjudicates transgressions through councils and penance rites, trains each generation of interpreters, and receives land grants, fees, and honorific standing from courts and households for these services. When devotional or egalitarian movements arise, absorbs them by classifying them as supplementary paths while retaining final interpretive say. Leaving would mean abandoning inherited learning and standing; adapting doctrine preserves both, so adaptation is the recurring move.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreter_class, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreter_class, beneficiary).

% Rules territory and commands armies under the teaching that fighting in a righteous cause is duty and heaven. Patronage flows from the court to the certifiers; in return the ruler's wars are consecrated and his order divinely warranted. Renouncing the framework would strip his legitimacy and hand rivals a doctrine-armed accusation, so he remains inside it even as its costs fall on his soldiers and subjects.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, kshatriya_warrior_rulers, beneficiary,
    powerful, generational, constrained, regional).

% Households of the three upper varnas hold ritual privilege, educational access, and rank above the service castes. They finance temples and priests, police marriage and dietary boundaries within their kin groups, and hand station to their children. Their standing is fused with the order itself: losing varna rank means social death for the lineage, so compliance is defended across generations even where individuals privately chafe.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, twice_born_upper_castes, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, twice_born_upper_castes, payer).

% Born into hereditary service obligations to landholding and twice-born households: agricultural labor, craft, domestic service. Classical teaching bars them from Vedic study and promises rebirth into higher station for faithful service in this one. Refusal brings penance, ostracism, or violence; leaving the order entirely means forfeiting community, marriage prospects, and ritual existence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, shudra_service_castes, payer,
    powerless, biographical, trapped, continental).

% Stand wholly outside the varna scheme as untouchables, assigned pollution-touching labor — leather, scavenging, corpse handling — and segregated in residence, barred from wells, temples, and schools. Boundary-crossing draws penalties ranging from fines to lethal violence, administered locally and ratified doctrinally. Escape historically meant flight, anonymous urban migration, or conversion, each of which severed every tie behind them.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_outcaste_communities, payer,
    powerless, biographical, trapped, continental).

% Fight under the teaching that battle in a righteous cause is duty and refusal is disgrace and sin — the discourse's own opening scene stages a warrior collapsing at the prospect and being argued back into the line. Commanders, priests, and kin invoke the text to move men into killing and dying; the soldier who lays down his arms bears infamy for himself and his line. His body is the arrangement's rawest payment, sanctified in advance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, dharmic_war_soldiers, payer,
    moderate, immediate, trapped, regional).

% Reform-current and nationalist readers who hold the field of Kurukshetra to be the human soul and its war the discipline of self-overcoming. They deny the text any warrant for caste hierarchy or literal slaughter and organize politically on that basis — nonviolence vows, campaigns of refusal, constructive village work. For most of the interval they stand outside the authorized interpretive conversation; their access runs through print, vernacular translation, and mass assembly rather than Sanskrit credential, and they accept beatings, prison, and ridicule as the price of that access.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_allegorical_readers, excluded,
    organized, generational, mobile, national).

% Buddhist, Jain, bhakti, and Sikh currents that deny birth-based rank or build communities outside it: mendicant orders that cut kin ties, saint-poets of artisan and outcaste origin preaching devotion without priesthood, new panths open to all comers. They object from outside the interpretive monopoly and absorb periodic persecution; their continuity across the interval constitutes the arrangement's standing dissent record.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, heterodox_bhakti_dissenters, excluded,
    organized, generational, mobile, continental).

% Academic philologists, historians of religion, and translators who reconstruct the text's composition layers, its epic context, and its reception across two millennia. They take no side in devotional dispute, publish in venues the tradition's councils do not control, and supply the evidentiary record the other seats argue over.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, modern_gita_hermeneuts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreter_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a vast agrarian civilization's division of labor and political-military order: varna and jati assign occupation, marriage, and ritual standing; the dharma framework organizes warfare under declared rules and gives households a cosmic rationale for role compliance across generations.
% TRANSFER_FUNCTION: Moves labor, produce, deference, and military service from shudra, outcaste, and war-bearing populations upward to priestly and ruling elites; moves interpretive authority and legitimacy to the hereditary scholar class, which converts textual mastery into social power, patronage, and standing.
% ABSENT_VOICES: Those bound by the duties were never asked: service-caste and outcaste communities had no standing to consent or refuse; soldiers who refused battle were branded sinners rather than heard; heterodox and bhakti dissenters objected from outside and their objections were absorbed as lesser paths or persecuted as heresy. They stand outside the conversation the interpreter class monopolizes — which is precisely this reading's complaint, and the reason its carriers built parallel channels of print, translation, and mass assembly.
% DISAPPEARANCE_RATIONALE: Remove the deployment overnight and the divine-warrant pillar of caste hierarchy falls: hereditary occupation, marriage rules, and purity codes lose their sanction; the interpreter class loses its authority returns; military recruitment loses its sanctified compulsion. Society would rearrange around contract, conscience, and voluntary association — which is exactly the rearrangement this reading advocates and which the subcontinent's mid-twentieth-century constitutional order formally began.
% FOUNDING_PROBLEM: Organizing large settled agrarian populations — assigning labor, regulating marriage and succession, staffing armies, stabilizing political authority — in epochs before impersonal markets, contract law, or mass literacy; the dharma framework answered coordination and legitimation in a single structure.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: economic historians of ancient and medieval South Asia corroborate the arrangement's origin in agrarian labor organization; Buddhist, Jain, and bhakti sources independently corroborate that its burdens fell on the lower orders; the mid-century constitution of the largest successor state formally abolishes untouchability — an external institutional attestation that the mandate is void. No attestation from service-caste or outcaste voices exists anywhere in the classical corpus itself; their silence in the record is itself signal, and this reading treats it as such.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.82: the arrangement takes hereditary labor, ritual deference, and — in war — life itself, across lifetimes and generations, from populations with no exit. Suppression 0.78: persistence rests on sin-doctrine, caste councils, ostracism, and interpretive monopoly, not on participant preference; it is authored as a raw structural property, and only extractiveness gets scaled by directionality and scope downstream. The suppression_requirement series traces an enforcement ratchet: as Buddhist, Jain, bhakti, Islamic-egalitarian, reformist, and nationalist dissent mounted, enforcement hardened to meet it rather than relaxing. Theater 0.48 and rising: as material enforcement grew costlier, a growing share of activity shifted to performing divine sanction — recitation, ritual, commentary — the sacral legwork that keeps obedience cheap; the rising ratio marks legitimation work displacing substantive function. Accessibility_collapse 0.6: internal alternatives (refusing varna duty, declining battle) are foreclosed as sin, but external exits — heterodox ordination, flight, conversion — stayed real, so alternatives never collapsed to natural-law completeness; this is a constructed order, not a law of nature. Resistance 0.55 reflects that continuous dissent record. The three series share one seven-point grid at four-unit steps (roughly dynastic-scale intervals, ~75-year units, from early smriti consolidation to the constitutional republic); all values are historical-record judgments, not instrument readings, and the coarseness is acknowledged rather than smoothed. Dynamics are a monotonic ratchet, not a cycle — no intermittent-reinforcement mechanism is alleged.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply and the engine computes the divergence from structural data rather than from this claim. From the interpreter-class seat the arrangement is a functioning order it staffs and profits from — subsidy side, negative effective burden. From the soldier's seat and the outcaste's seat the same verses are a machine that consumes bodies — full-target side. From the orthodox believer's seat the whole is sacred duty coordination and this story's metrics read as calumny; the universalist seat splits the difference, opening access while keeping divine command over conscience. The authored snare claim records only this reading's judgment of the standing deployment and adjudicates nothing about the sibling seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the interpreter class, warrior rulers, and twice-born households near the beneficiary end; the interpreter class additionally holds arbitrage-grade exit — it can reclassify dissent as a lesser path and adapt doctrine to preserve position — which damps its burden toward subsidy. Victim declarations place service castes, outcaste communities, and war soldiers near the target end, and trapped exits push them to the full-target pole: a soldier cannot resign from a sin, and a caste cannot be resigned from at all. Twice-born households are nominally beneficiaries but identity_locked — status and lineage are fused with the order — so lock-in deepens their investment rather than damping it. Continental scope for most of the interval degrades verification and amplifies the effective burden on targets relative to what local enforcement alone could achieve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating labor, marriage, succession, and army-raising in large agrarian polities before contract law and mass literacy — was real and genuinely solved for centuries; that is what makes the cover durable and why the status is contested rather than dead. Under modern conditions markets, constitutions, and citizenship answer the same problems, and the residue is extraction in old vestments. The mismatch consumer reads founding_problem_status x disappearance_verdict: here the verdict is world_rearranges with contested status — short of the dead-plus-rearranges zombie signature, but close enough that the flag's neighborhood matters. The rising extractiveness series is the accumulation signature: rent layered onto coordination while the frame presented itself as eternal dharma. Had this story claimed mountain, the beneficiary declarations would route it through false-summit evaluation; stripping the naturality claim off a constructed order is in fact this reading's central move — Kurukshetra as metaphor dissolves the claim that the text mandates any earthly battlefield at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gita_kernel_reading_delta,
    'This constraint is one reading of kernel gita_kurukshetra_discourse. What happens to the victim set and to epsilon under the sibling readings?',
    'Compile and compare the sibling files gita_kurukshetra_discourse__orthodox_literal_reading and gita_kurukshetra_discourse__universalist_devotional_reading: the orthodox reading relocates victims to duty-refusers and enemies in righteous war and drives epsilon toward the coordination-cost floor; the universalist reading keeps anti-caste access but reinstates divine-command authority over conscience.',
    'Switching readings changes the victim set wholesale and swings the computed classification between snare, duty-coordination, and partially-redeemed access — the kernel''s classification is reading-relative, not text-intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gita_kernel_reading_delta, conceptual, 'Committer structure: this story instantiates the Gandhian-allegorical member of a three-reading kernel family; siblings alter victims, epsilon, and type.').

omega_variable(
    allegorical_intent_vs_modern_imposition,
    'Does the allegorical, ahimsa-centered reading recover the discourse''s compositional intent, or impose a modern pacifist ethic on a text embedded in a heroic-war epic?',
    'Philology and reception history: analysis of the epic war-frame surrounding the discourse, the earliest commentary strata (classical commentators read the duty to fight literally), and the modern genealogy of allegorical pacifist readings culminating in Gandhi''s vernacular commentary, in which he acknowledged he was bending the text to ahimsa.',
    'If the reading is a modern imposition, its authority over the kernel rests on conscience rather than textual recovery and its foreclosure edge against the literal reading softens; if it is recovery, the literal mandate loses textual ground and this reading''s indictment strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_intent_vs_modern_imposition, empirical, 'Whether allegorical reading is recovery of compositional intent or modern moral projection.').

omega_variable(
    residual_coordination_function,
    'Did the standing arrangement retain genuine coordination output — role clarity, welfare provisioning, war-limitation rules — that this reading''s indictment discounts?',
    'Historical-economic accounting: compare order and welfare outcomes in varna-ordered regions against contemporaneous societies with alternative institutions; weigh the classical war-restraint rules against documented conduct in actual campaigns.',
    'A substantial residual function would push the arrangement from snare toward tangled_rope — coordination carrying asymmetric extraction — and would soften the total indictment this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_function, conceptual, 'Uncertainty over residual coordination value beneath the extraction this reading indicts.').

omega_variable(
    extraction_composition_caste_vs_war,
    'How much of the measured extraction is caste-economy extraction (compulsory hereditary labor and exclusion) versus sanctified-war mortality, and do the two components move together across the interval?',
    'Decomposed series: demographic and economic histories of bound labor and untouchability alongside war-mortality estimates under the righteous-war doctrine.',
    'If war-sanctification dominates, remedies address military ethics and the victim set widens to combatant populations; if caste-economy dominates, the war component is rhetorical reinforcement and the victim set narrows to laboring castes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_composition_caste_vs_war, empirical, 'Composition of the arrangement''s extracted value across caste labor and war deaths.').

omega_variable(
    internalized_vs_structural_compliance,
    'How much compliance rests on external sanction (councils, ostracism, force) versus internalized karma-dharma conviction that faithful service in this birth lifts the next?',
    'Post-sanction trajectory test: observe adherence in periods and regions where enforcement capacity lapsed (colonial legal disruption, anonymous urban migration). If adherence persists without enforcement, internalization carries it; if it collapses, structure carried it.',
    'High internalization raises the arrangement''s hold beyond its enforcement machinery and predicts slow decay after legal abolition — untouchability persisted long after the 1950 constitutional ban — whereas purely structural suppression predicts rapid release.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_compliance, empirical, 'Split between structural and internalized mechanisms sustaining compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_gandhian_reading_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t0, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t4, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t4, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t8, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t8, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t12, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t12, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t16, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t16, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t20, observed).
narrative_ontology:measurement(gita_gandhian_reading_tr_t24, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(gita_gandhian_reading_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(gita_gandhian_reading_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t0, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t4, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 4, 0.66).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t4, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t8, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 8, 0.71).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t8, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t12, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t12, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t16, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t16, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t20, observed).
narrative_ontology:measurement(gita_gandhian_reading_be_t24, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(gita_gandhian_reading_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_gandhian_reading_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t0, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t4, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t4, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t8, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t8, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t12, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 12, 0.73).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t12, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t16, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t16, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t20, observed).
narrative_ontology:measurement(gita_gandhian_reading_su_t24, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(gita_gandhian_reading_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, resource_allocation).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel gita_kurukshetra_discourse per the epsilon-invariance principle: the colloquial label 'what the Gita teaches' covers three structurally distinct claims with distinct victim sets and epsilon values. This file authors the Gandhian-allegorical instantiation (epsilon 0.82 over the standing orthodox deployment). The orthodox-literal sibling authors low epsilon over the same referent; the universalist-devotional sibling authors an intermediate value. Edges run both directions: the literal reading historically supplied the arrangement this story indicts, and this reading's success creates downstream legitimacy pressure on both siblings without resolving either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
